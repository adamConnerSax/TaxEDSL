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
  , Jurisdiction(..)
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
  , adjustedGrossIncome
  , modifiedAGI
  , flooredNetNonInvestmentIncome
  , grossNonInvestmentIncome
  , flooredNetInvestmentIncome
  , grossInvestmentIncome
  , stateCapGainRate
  , flow
  , brackets
  , standardDeduction
  , saltCap
  , fedCapGainInfo
  , medSurtaxInfo
  , applyBrackets
  , fedCapGain
  , medicareSurtax
  , marginalRate
  , taxReaderProgram
  , runTaxMonad
  )  where

import           TaxEDSL.Money

import           Control.Monad.Free   (Free (..), liftF)
import           Control.Monad.Reader (MonadReader, Reader, asks, runReader)
import qualified Data.Array           as A
import           Data.Ix              (Ix)

import           Data.Foldable        (foldl')
import           Prelude              hiding ((<*>))

-- These will come from here
data TaxType = NonPayrollIncome | OrdinaryIncome | CapitalGain | Dividend | ExemptInterest | Inheritance | StateAndLocal deriving (Show, Enum, Eq, Bounded, Ord, Ix)
data BracketType = Federal | State | Local | Payroll | Estate deriving (Show, Bounded, Eq, Ord, Enum, Ix) -- Payroll does Social Security and Medicare taxes
data Jurisdiction = FederalJ | StateJ | LocalJ deriving (Show, Bounded, Eq, Ord, Enum, Ix)


-- These will be exported so things can be converted to them
data TaxBracketM a = BracketM (Money a) (Money a) a | TopBracketM (Money a) a deriving (Show)
data TaxBracketsM a = TaxBracketsM ![TaxBracketM a] deriving (Show) -- we don't expose this constructor
data CapGainBandM a = CapGainBandM { marginalRateM :: a, capGainRateM :: a } deriving (Show)
data FedCapitalGainsM a = FedCapitalGainsM { topRateM :: a, bandsM :: [CapGainBandM a] } deriving (Show)
data MedicareSurtaxM a = MedicareSurtaxM { netInvRateM :: a, magiThresholdM :: Money a } deriving (Show) -- payroll piece included in payroll bracket

data TaxRulesM a = TaxRulesM { _trBrackets           :: A.Array BracketType (TaxBracketsM a),
                               _trStandardDeductions :: A.Array Jurisdiction (Money a),
                               _trSALTCap            :: Maybe (Money a),
                               _trFedCapGain         :: FedCapitalGainsM a,
                               _trMedSurtax          :: MedicareSurtaxM a,
                               _trStateCapGain       :: a
                             } deriving (Show)

data TaxFlow a = TaxFlow { inFlow :: Money a, deductions :: Money a } deriving (Show)

netFlow :: Fractional a => TaxFlow a -> Money a
netFlow (TaxFlow i d) = i |-| d

flooredNetFlow :: (Ord a, Fractional a) => TaxFlow a -> Money a
flooredNetFlow tf = max moneyZero (netFlow tf)

--instance Functor TaxFlow where
--  fmap f (TaxFlow i d) = TaxFlow (f i) (f d)

-- DSL
data TaxEDSL b a where
  Flow              :: Fractional b => TaxType -> (TaxFlow b -> a) -> TaxEDSL b a -- get current flow details in bucket
  FedCapGainInfo    :: Fractional b => (FedCapitalGainsM b -> a) -> TaxEDSL b a -- get the cap gain rate from AGI
  StateCapGainRate  :: Fractional b => (b -> a) -> TaxEDSL b a -- get the state cap gain rate
  MedSurtaxInfo     :: Fractional b => (MedicareSurtaxM b -> a) -> TaxEDSL b a -- compute the medicare surtax (payroll and inv income) from MAGI and inv invcome
  Brackets          :: Fractional b => BracketType -> (TaxBracketsM b -> a) -> TaxEDSL b a -- get bracket.  We will not export access to this
  StandardDeduction :: Fractional b => Jurisdiction -> (Money b -> a) -> TaxEDSL b a -- get standard deduction
  SALTCap           :: Fractional b => (Maybe (Money b) -> a) -> TaxEDSL b a

instance Functor (TaxEDSL b) where
  fmap f (Flow c g)              = Flow c  (f . g)
  fmap f (FedCapGainInfo g)      = FedCapGainInfo (f . g)
  fmap f (StateCapGainRate g)    = StateCapGainRate (f . g)
  fmap f (MedSurtaxInfo g)       = MedSurtaxInfo (f . g)
  fmap f (Brackets bt g)         = Brackets bt (f . g)
  fmap f (StandardDeduction j g) = StandardDeduction j (f . g)
  fmap f (SALTCap g)             = SALTCap (f . g)

type TaxComputation b = Free (TaxEDSL b)

-- basics
flow :: Fractional b => TaxType -> TaxComputation b (TaxFlow b)
flow c = liftF (Flow c id)

fedCapGainInfo :: Fractional b => TaxComputation b (FedCapitalGainsM b)
fedCapGainInfo = liftF (FedCapGainInfo id)

stateCapGainRate :: Fractional b => TaxComputation b b
stateCapGainRate = liftF (StateCapGainRate id)

medSurtaxInfo :: Fractional b => TaxComputation b (MedicareSurtaxM b)
medSurtaxInfo = liftF (MedSurtaxInfo id)

brackets :: Fractional b => BracketType -> TaxComputation b (TaxBracketsM b)
brackets bt = liftF (Brackets bt id)

standardDeduction :: Fractional b => Jurisdiction -> TaxComputation b (Money b)
standardDeduction j = liftF (StandardDeduction j id)

saltCap :: Fractional b => TaxComputation b (Maybe (Money b))
saltCap = liftF (SALTCap id)

-- helpers
-- specialize a fold for combining tax flows
onTaxFlows :: Fractional b => (TaxFlow b -> c) -> (c -> c -> c) -> c -> [TaxType] -> TaxComputation b c
onTaxFlows fEach combine initial types = foldl' combine initial <$> (sequence . fmap (fmap fEach . flow) $ types)

-- specialize further to the case of (|+|) starting at 0
sumTaxFlows :: Fractional b => (TaxFlow b -> Money b) -> [TaxType] -> TaxComputation b (Money b)
sumTaxFlows toMoney types = onTaxFlows toMoney (|+|) moneyZero types

adjustedGrossIncome :: Fractional b => TaxComputation b (Money b)
adjustedGrossIncome = sumTaxFlows inFlow [OrdinaryIncome, NonPayrollIncome, CapitalGain, Dividend]

modifiedAGI :: Fractional b => TaxComputation b (Money b)
modifiedAGI = sumTaxFlows inFlow [OrdinaryIncome, NonPayrollIncome, CapitalGain, Dividend, ExemptInterest]

grossNonInvestmentIncome :: forall b.(Ord b, Fractional b) => TaxComputation b (Money b)
grossNonInvestmentIncome = sumTaxFlows inFlow [OrdinaryIncome, NonPayrollIncome]

flooredNetNonInvestmentIncome :: forall b.(Ord b, Fractional b) => TaxComputation b (Money b)
flooredNetNonInvestmentIncome = sumTaxFlows flooredNetFlow [OrdinaryIncome, NonPayrollIncome]

grossInvestmentIncome :: forall b.(Ord b, Fractional b) => TaxComputation b (Money b)
grossInvestmentIncome = sumTaxFlows inFlow [CapitalGain, Dividend, ExemptInterest]

flooredNetInvestmentIncome :: forall b.(Ord b, Fractional b) => TaxComputation b (Money b)
flooredNetInvestmentIncome = sumTaxFlows flooredNetFlow [CapitalGain, Dividend]


applyBrackets :: forall b.(Ord b, Fractional b) => BracketType -> Money b -> TaxComputation b (Money b) -- forall here for scopedTypeVars needed for (Money 0)
applyBrackets bt inc = do
  (TaxBracketsM bkts) <- brackets bt
  let taxFromBracket :: Money b -> TaxBracketM b -> Money b
      taxFromBracket x (BracketM bot top r) = if (x <= bot) then moneyZero else r |*| (if x > top then top |-| bot else x |-| bot)
      taxFromBracket x (TopBracketM bot r) = if (x <= bot) then moneyZero else r |*| (x |-| bot)
      go :: Money b -> [TaxBracketM b] -> Money b
      go x bs = foldl' (\tot bkt -> tot |+| (taxFromBracket x bkt)) moneyZero bs
  return $ go inc bkts

fedCapGain :: (Ord b, Fractional b) => TaxComputation b (Money b)
fedCapGain = do
  margRate <- (adjustedGrossIncome >>= trueMarginalRate Federal)
  (FedCapitalGainsM topRate bands) <- fedCapGainInfo
  let inBand margTaxRate (CapGainBandM bandRate _) = (margTaxRate <= bandRate)
      taxRate (CapGainBandM _ rate) = rate
  -- we start at top rate and go lower if the given marginal rate is lower than the band rate.  But we shouldn't go up if bands out of order.
  let f cgTaxRate band = if inBand margRate band then min (taxRate band) cgTaxRate else cgTaxRate
      cgr = foldl' f topRate bands
  invInc <- flooredNetInvestmentIncome
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

medicareSurtax :: forall b. (Fractional b, Ord b) => TaxComputation b (Money b)
medicareSurtax = do
  let moneyZ = (Money 0) :: Money b
  (MedicareSurtaxM rate magiThreshold) <- medSurtaxInfo
  magi <- modifiedAGI
  invInc <- flooredNetInvestmentIncome
  let medSTaxable = min invInc (max moneyZ (magi |-| magiThreshold))
  return $ rate |*| medSTaxable

-- Here is one possible implementation and interpreter using Reader
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
  Free (MedSurtaxInfo g) -> do
    mst <- asks (_trMedSurtax . _teRules)
    taxReaderProgram $ g mst -- unimplemented for now
  Free (FedCapGainInfo g) -> do
    fcg <- asks (_trFedCapGain . _teRules)
    taxReaderProgram $ g fcg
  Free (StateCapGainRate g) -> do
    scg <- asks (_trStateCapGain . _teRules)
    taxReaderProgram $ g scg
  Free (Brackets bt g) -> do
    bkts <- asks (_trBrackets . _teRules)
    taxReaderProgram . g $ bkts A.! bt
  Free (StandardDeduction j g) -> do
    deds <- asks (_trStandardDeductions . _teRules)
    taxReaderProgram . g $ deds A.! j
  Free (SALTCap g) -> do
    cap <- asks (_trSALTCap . _teRules)
    taxReaderProgram $ g cap
  Pure x -> TaxMonad $ return x






