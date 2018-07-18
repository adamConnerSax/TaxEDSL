{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
module TaxEDSL.Core
  (
    TaxFlow(..)
  , TaxType(..)
  , BracketType(..)
  , TaxBracketsM(..)
  , TaxBracketM(..)
  , CapGainBandM(..)
  , FedCapitalGainsM(..)
  , MedicareSurtaxM(..)
  , TaxRulesM(..)
  , TaxComputation
  , TaxFlows
  , TaxEnv(..)
  , netFlow
  , flooredNetFlow
  , flooredNetCGAndDivs
  , grossIncome
  , flooredNetIncome
  , stateCapGainRate
  , flow
  , brackets
  , fedCapGain
  , medSurtax
  , applyBrackets
  , applyFedCapGain
  , applyMedSurTax
  , marginalRate
  , taxReaderProgram
  , runTaxMonad
  )  where

import           TaxEDSL.Money

import           Control.Monad        (liftM2)
import           Control.Monad.Free   (Free (..), liftF)
import           Control.Monad.Reader (MonadReader, Reader, asks, runReader)
import qualified Data.Array           as A
import           Data.Ix              (Ix)
import           Data.Map             (Map (..))

import           Data.Foldable        (foldl')
import           Prelude              hiding ((<*>))

-- These will come from here
data TaxType = NonPayrollIncome | OrdinaryIncome | CapitalGain | Dividend | Inheritance | Exempt deriving (Show, Enum, Eq, Bounded, Ord, Ix)
data BracketType = Federal | State | Local | Payroll | Estate deriving (Show, Bounded, Eq, Ord, Enum, Ix) -- Payroll does Social Security and Medicare taxes


-- These will be exported so things can be converted to them
data TaxBracketM a = BracketM (Money a) (Money a) a | TopBracketM (Money a) a deriving (Show)
data TaxBracketsM a = TaxBracketsM ![TaxBracketM a] deriving (Show) -- we don't expose this constructor
data CapGainBandM a = CapGainBandM { marginalRateM :: a, capGainRateM :: a } deriving (Show)
data FedCapitalGainsM a = FedCapitalGainsM { topRateM :: a, bandsM :: [CapGainBandM a] } deriving (Show)
data MedicareSurtaxM a = MedicareSurtaxM { payrollRateM :: a, netInvRateM :: a, magiThresholdM :: Money a } deriving (Show) -- payrollRate ignored for now, included in payroll bracket

data TaxRulesM a = TaxRulesM { _trBrackets     :: A.Array BracketType (TaxBracketsM a),
                               _trFedCapGain   :: FedCapitalGainsM a,
                               _trMedSurtax    :: MedicareSurtaxM a,
                               _trStateCapGain :: a
                             } deriving (Show)

data TaxFlow a = TaxFlow { inFlow :: Money a, deductions :: Money a } deriving (Show)

netFlow :: Fractional a => TaxFlow a -> Money a
netFlow (TaxFlow i d) = i |-| d

flooredNetFlow :: forall a.(Ord a, Fractional a) => TaxFlow a -> Money a
flooredNetFlow tf = let moneyZ = (Money 0) :: Money a in max moneyZ (netFlow tf)

--instance Functor TaxFlow where
--  fmap f (TaxFlow i d) = TaxFlow (f i) (f d)

-- DSL
data TaxEDSL b a where
  Flow             :: Fractional b => TaxType -> (TaxFlow b -> a) -> TaxEDSL b a -- get current flow details in bucket
  FedCapGain       :: Fractional b => (FedCapitalGainsM b -> a) -> TaxEDSL b a -- get the cap gain rate from AGI
  StateCapGainRate :: Fractional b => (b -> a) -> TaxEDSL b a -- get the state cap gain rate
  MedSurtax        :: Fractional b => (MedicareSurtaxM b -> a) -> TaxEDSL b a -- compute the medicare surtax (payroll and inv income) from MAGI and inv invcome
  Brackets         :: Fractional b => BracketType -> (TaxBracketsM b -> a) -> TaxEDSL b a -- get bracket.  We will not export access to this

instance Functor (TaxEDSL b) where
  fmap f (Flow c g)           = Flow c  (f . g)
  fmap f (FedCapGain g)       = FedCapGain (f . g)
  fmap f (StateCapGainRate g) = StateCapGainRate (f . g)
  fmap f (MedSurtax g)        = MedSurtax (f . g)
  fmap f (Brackets bt g)      = Brackets bt (f . g)

type TaxComputation b = Free (TaxEDSL b)

-- basics
flow :: Fractional b => TaxType -> TaxComputation b (TaxFlow b)
flow c = liftF (Flow c id)

brackets :: Fractional b => BracketType -> TaxComputation b (TaxBracketsM b)
brackets bt = liftF (Brackets bt id)

fedCapGain :: Fractional b => TaxComputation b (FedCapitalGainsM b)
fedCapGain = liftF (FedCapGain id)

stateCapGainRate :: Fractional b => TaxComputation b b
stateCapGainRate = liftF (StateCapGainRate id)

medSurtax :: Fractional b => TaxComputation b (MedicareSurtaxM b)
medSurtax = liftF (MedSurtax id)

-- helpers
flooredNetIncome :: forall b.(Ord b, Fractional b) => TaxComputation b (Money b)
flooredNetIncome = liftM2 (|+|) (flooredNetFlow <$> flow OrdinaryIncome) (flooredNetFlow <$> flow NonPayrollIncome)

grossIncome :: forall b.(Ord b, Fractional b) => TaxComputation b (Money b)
grossIncome = liftM2 (|+|) (inFlow <$> flow OrdinaryIncome) (inFlow <$> flow NonPayrollIncome)

flooredNetCGAndDivs :: forall b.(Ord b, Fractional b) => TaxComputation b (Money b)
flooredNetCGAndDivs = liftM2 (|+|) (flooredNetFlow <$> flow CapitalGain) (flooredNetFlow <$> flow Dividend)

applyBrackets :: forall b.(Ord b, Fractional b) => BracketType -> Money b -> TaxComputation b (Money b) -- forall here for scopedTypeVars needed for (Money 0)
applyBrackets bt inc = do
  (TaxBracketsM bkts) <- brackets bt
  let moneyZ = (Money 0) :: Money b
      taxFromBracket :: Money b -> TaxBracketM b -> Money b
      taxFromBracket x (BracketM bot top r) = if (x <= bot) then moneyZ else r |*| (if x > top then top |-| bot else x |-| bot)
      taxFromBracket x (TopBracketM bot r) = if (x <= bot) then moneyZ else r |*| (x |-| bot)
      go :: Money b -> [TaxBracketM b] -> Money b
      go x bs = foldl' (\tot bkt -> tot |+| (taxFromBracket x bkt)) moneyZ bs
  return $ go inc bkts

applyFedCapGain :: (Ord b, Fractional b) => Money b -> Money b -> TaxComputation b (Money b)
applyFedCapGain agi invInc = do
  margRate <- trueMarginalRate Federal agi
  (FedCapitalGainsM topRate bands) <- fedCapGain
  let inBand margTaxRate (CapGainBandM bandRate _) = (margTaxRate <= bandRate)
      taxRate (CapGainBandM _ rate) = rate
  -- we start at top rate and go lower if the given marginal rate is lower than the band rate.  But we shouldn't go up if bands out of order.
  let f cgTaxRate band = if inBand margRate band then min (taxRate band) cgTaxRate else cgTaxRate
      cgr = foldl' f topRate bands
  return $ cgr |*| invInc

marginalRate :: (Ord b, Fractional b) => BracketType -> Money b -> TaxComputation b b
marginalRate bt inc = do
  (TaxBracketsM bkts) <- brackets bt
  let inBracket x (BracketM b _ _)  = (x >= b)
      inBracket x (TopBracketM b _) = (x >= b)
      rate (BracketM _ _ r)  = r
      rate (TopBracketM _ r) = r
  return $ foldl' (\mr bkt -> if inBracket inc bkt then rate bkt else mr) 0 bkts

trueMarginalRate :: (Ord b, Fractional b) => BracketType -> Money b -> TaxComputation b b
trueMarginalRate bt (Money x) = do
  (Money y0) <- applyBrackets bt (Money x)
  (Money y1) <- applyBrackets bt (Money (x+1))
  return (y1 - y0)


-- zero for now to match old computation
applyMedSurTax :: forall b. Fractional b => Money b -> Money b -> TaxComputation b (Money b)
applyMedSurTax magi invInc = do
  let moneyZ = (Money 0) :: Money b
  (MedicareSurtaxM payrollRate invIncRate magiThreshold) <- medSurtax
  return moneyZ

-- Here is one possible implementation and interpreter using Reader and State
-- these could also be hard-wired or come from a DB or a SOAP query or a form
type TaxFlows b = A.Array TaxType (TaxFlow b)
data TaxEnv b =  TaxEnv { _teRules :: TaxRulesM b, _teFlows :: TaxFlows b }

newtype TaxMonad b a = TaxMonad { unTaxMonad :: Reader (TaxEnv b) a } deriving (Functor, Applicative, Monad, MonadReader (TaxEnv b))

runTaxMonad :: TaxMonad b a -> TaxEnv b -> a
runTaxMonad tm = runReader (unTaxMonad tm)

taxReaderProgram :: Fractional b => Free (TaxEDSL b) a -> TaxMonad b a
taxReaderProgram prog = case prog of
  Free (Flow c g) -> do
    taxFlowArray <- asks _teFlows
    taxReaderProgram . g $ taxFlowArray A.! c
  Free (Brackets bt g) -> do
    bkts <- asks (_trBrackets . _teRules)
    taxReaderProgram . g $ bkts A.! bt
  Free (MedSurtax g) -> do
    mst <- asks (_trMedSurtax . _teRules)
    taxReaderProgram $ g mst -- unimplemented for now
  Free (FedCapGain g) -> do
    fcg <- asks (_trFedCapGain . _teRules)
    taxReaderProgram $ g fcg
  Free (StateCapGainRate g) -> do
    scg <- asks (_trStateCapGain . _teRules)
    taxReaderProgram $ g scg
  Pure x -> TaxMonad $ return x






