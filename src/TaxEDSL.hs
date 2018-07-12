{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
module TaxEDSL where

import           Money

import           Control.Monad.Free   (Free (..), liftF)
import           Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import           Control.Monad.State  (MonadState, State, get, put, runState)
import qualified Data.Array           as A
import           Data.Ix              (Ix)
import           Data.Map             (Map (..))

import           Data.Foldable        (foldl')
import           Prelude              hiding ((<*>))

-- These will come from here
data TaxCategory = PayrollIncome | OrdinaryIncome | CapitalGain | Dividend | Inheritance | Exempt deriving (Show, Enum, Eq, Bounded, Ord, Ix)
data FilingStatus = Single | MarriedFilingJointly deriving (Show, Read, Enum)
data BracketType = Federal | State | Local | Payroll | Estate deriving (Show, Bounded, Eq, Ord, Enum, Ix) -- Payroll does Social Security and Medicare taxes


-- These will be exported so things can be converted to them
data TaxBracketM a = BracketM (Money a) (Money a) a | TopBracketM (Money a) a deriving (Show)
data TaxBracketsM a = TaxBracketsM ![TaxBracketM a] deriving (Show) -- we don't expose this constructor
data CapGainBandM a = CapGainBandM { marginalRateM :: a, capGainRateM :: a } deriving (Show)
data FedCapitalGainsM a = FedCapitalGainsM { topRateM :: a, bandsM :: [CapGainBandM a] } deriving (Show)
data MedicareSurtaxM a = MedicareSurtaxM { payrollRateM :: a, netInvRateM :: a, magiThresholdM :: Money a } deriving (Show) -- payrollRate ignored for now, included in payroll bracket

data TaxRulesM a = TaxRulesM { _trBrackets     :: A.Array BracketType (TaxBracketsM a),
                               _trFedCapGain          :: FedCapitalGainsM a,
                               _trMedSurtax    :: MedicareSurtaxM a,
                               _trStateCapGain :: a
                             } deriving (Show)

data TaxFlow a = TaxFlow { inFlow :: Money a, deductions :: Money a } deriving (Show)

--instance Functor TaxFlow where
--  fmap f (TaxFlow i d) = TaxFlow (f i) (f d)

-- DSL
data TaxEDSL b a where
  Flow       :: Fractional b => TaxCategory -> (TaxFlow b -> a) -> TaxEDSL b a -- get current flow details in bucket
  Deduct     :: Fractional b => TaxCategory -> Money b -> a -> TaxEDSL b a -- add a deduction to a bucket.  Used to deduct, e.g., state & local taxes from fed AGI
  FedCapGain :: Fractional b => (FedCapitalGainsM b -> a) -> TaxEDSL b a -- get the cap gain rate from AGI
  MedSurtax  :: Fractional b => (MedicareSurtaxM b -> a) -> TaxEDSL b a -- compute the medicare surtax (payroll and inv income) from MAGI and inv invcome
  Brackets   :: Fractional b => BracketType -> (TaxBracketsM b -> a) -> TaxEDSL b a -- get bracket.  We will not export access to this

instance Functor (TaxEDSL b) where
  fmap f (Flow c g)             = Flow c  (f . g)
  fmap f (Deduct c deduction a) = Deduct c deduction (f a)
  fmap f (FedCapGain g)         = FedCapGain (f . g)
  fmap f (MedSurtax g)          = MedSurtax (f . g)
  fmap f (Brackets bt g)        = Brackets bt (f . g)

type TaxComputation b = Free (TaxEDSL b)

-- basics
flow :: Fractional b => TaxCategory -> TaxComputation b (TaxFlow b)
flow c = liftF (Flow c id)

deduct :: Fractional b => TaxCategory -> Money b -> TaxComputation b ()
deduct c x = liftF (Deduct c x ())

brackets :: Fractional b => BracketType -> TaxComputation b (TaxBracketsM b)
brackets bt = liftF (Brackets bt id)

fedCapGain :: Fractional b => TaxComputation b (FedCapitalGainsM b)
fedCapGain = liftF (FedCapGain id)

medSurtax :: Fractional b => TaxComputation b (MedicareSurtaxM b)
medSurtax = liftF (MedSurtax id)

-- some helpers!
applyBrackets :: forall b.(Ord b, Fractional b) => BracketType -> Money b -> TaxComputation b (Money b) -- forall here for scopedTypeVars needed for (Money 0)
applyBrackets bt inc = do
  (TaxBracketsM bkts) <- brackets bt
  let moneyZ = (Money 0) :: Money b
      taxFromBracket :: Money b -> TaxBracketM b -> Money b
      taxFromBracket x (BracketM bot top r) = if (x <= bot) then moneyZ else r <*> (if x > top then top <-> bot else x <-> bot)
      taxFromBracket x (TopBracketM bot r) = if (x <= bot) then moneyZ else r <*> (x <-> bot)
      go :: Money b -> [TaxBracketM b] -> Money b
      go x bs = foldl' (\tot bkt -> tot <+> (taxFromBracket x bkt)) moneyZ bs
  return $ go inc bkts

applyFedCapGain :: (Ord b, Fractional b) => Money b -> Money b -> TaxComputation b (Money b)
applyFedCapGain agi invInc = do
  margRate <- marginalRate Federal agi
  (FedCapitalGainsM topRate bands) <- fedCapGain
  -- we start at top rate and go lower if the given marginal rate is lower than the band rate.  But we shouldn't go up if bands out of order.
  let f margTaxRate cgTaxRate (CapGainBandM bandRate bandCapGainRate) = if (margTaxRate <= bandRate) then min bandCapGainRate cgTaxRate else cgTaxRate
      cgr = foldl' (f margRate) topRate bands
  return $ cgr <*> invInc

marginalRate :: (Ord b, Fractional b) => BracketType -> Money b -> TaxComputation b b
marginalRate bt inc = do
  (TaxBracketsM bkts) <- brackets bt
  let inBracket x (BracketM b _ _)  = if (x >= b) then True else False
      inBracket x (TopBracketM b _) = if (x >= b) then True else False
      rate (BracketM _ _ r)  = r
      rate (TopBracketM _ r) = r
  return $ foldl' (\mr bkt -> if inBracket inc bkt then rate bkt else mr) 0 bkts

-- zero for now to match old computation
applyMedSurTax :: forall b. Fractional b => Money b -> Money b -> TaxComputation b (Money b)
applyMedSurTax magi invInc = do
  let moneyZ = (Money 0) :: Money b
  (MedicareSurtaxM payrollRate invIncRate magiThreshold) <- medSurtax
  return moneyZ


-- Here is an implementation and interpreter using Reader and State
-- these could also be hard-wired or come from a DB or a SOAP query or a form
type FlowState b = A.Array TaxCategory (TaxFlow b)
type TaxEnv b =  TaxRulesM b

newtype TaxMonad b a = TaxMonad { unTaxMonad :: ReaderT (TaxEnv b) (State (FlowState b)) a } deriving (Functor, Applicative, Monad, MonadState (FlowState b), MonadReader (TaxEnv b))

runTaxMonad :: TaxEnv b -> FlowState b -> TaxMonad b a -> (a, FlowState b)
runTaxMonad e s tm = runState (runReaderT (unTaxMonad tm) e) s

taxStateProgram :: Fractional b => Free (TaxEDSL b) (Money b) -> TaxMonad b (Money b)
taxStateProgram prog = case prog of
  Free (Flow c g) -> do
    taxFlowArray <- get
    let taxFlow = taxFlowArray A.! c
    taxStateProgram $ g taxFlow
  Free (Deduct c x y) -> do
    taxFlowArray <- get
    let taxFlow = taxFlowArray A.! c
        newTaxFlow = (\(TaxFlow i d) -> (TaxFlow i (d <+> x))) taxFlow
        newTaxFlowArray = taxFlowArray A.// [(c,newTaxFlow)]
    put newTaxFlowArray
    taxStateProgram y
  Free (Brackets bt g) -> do
    bkts <- asks _trBrackets
    taxStateProgram $ g (bkts A.! bt)
  Free (MedSurtax g) -> do
    mst <- asks _trMedSurtax
    taxStateProgram $ g mst -- unimplemented for now
  Free (FedCapGain g) -> do
    fcg <- asks _trFedCapGain
    taxStateProgram $ g fcg
  Pure x -> TaxMonad $ return x

-- Here is an example

testFlows :: Fractional b => FlowState b
testFlows = let f = Money in A.listArray (minBound, maxBound) (repeat (TaxFlow (f 0.0) (f 0.0))) A.//
  [
    (PayrollIncome, TaxFlow (f 100.0) (f 10.0))
  , (CapitalGain, TaxFlow (f 10) (f 0))
  ]

-- NB: the forall awkwardness here only arises because we have scalar constants.  In a real application we would likely not have those

stateTax :: forall b.Fractional b => TaxComputation b (Money b)
stateTax = do
  payroll <- flow PayrollIncome
  capGain <- flow CapitalGain
  inheritance <- flow Inheritance
  let net (TaxFlow i d) = i <-> d
      tax = ((realToFrac 0.1) :: b) <*> ( net payroll <+> net capGain <+> net inheritance)
  deduct PayrollIncome tax
  return tax

fedTax :: forall b.Fractional b => TaxComputation b (Money b)
fedTax = do
  payroll <- flow PayrollIncome
  capGain <- flow CapitalGain
  inheritance <- flow Inheritance
  let net (TaxFlow i d) = i <-> d
      tax = ((fromRational 0.2) :: b) <*> ( net payroll <+> net capGain <+> net inheritance)
  return tax

allTax :: Fractional b => TaxComputation b (Money b)
allTax = do
  state <- stateTax
  fed <- fedTax
  return (state <+> fed)



