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
import           Data.Array
import           Data.Ix
import           Data.Map             (Map (..))

import           Prelude              hiding ((<*>))

-- These will come from here
data TaxCategory = PayrollIncome | OrdinaryIncome | CapitalGain | Dividend | Inheritance | Exempt deriving (Show, Enum, Eq, Bounded, Ord, Ix)
data FilingStatus = Single | MarriedFilingJointly deriving (Show, Read, Enum)

-- These will be exported so things can be converted to them
data TaxBracketM a = BracketM (Money a) (Money a) a | TopBracket (Money a) a deriving (Show)
data TaxBracketsM a = TaxBracketsM ![TaxBracketM a] deriving (Show) -- we don't expose this constructor
data CapGainBandM a = CapGainBandM { marginalRateM :: a, capGainRateM :: a } deriving (Show)
data FedCapitalGainsM a = FedCapitalGainsM { topRateM :: a, bandsM :: [CapGainBandM a] } deriving (Show)
data MedicareSurtaxM a = MedicareSurtaxM { rateM :: a, magiThreshold :: Money a } deriving (Show)

data TaxRulesM a = TaxRules {_trFederal      :: TaxBracketsM a,
                             _trPayroll      :: TaxBracketsM a,
                             _trEstate       :: TaxBracketsM a,
                             _trFCG          :: FedCapitalGainsM a,
                             _trMedTax       :: MedicareSurtaxM a,
                             _trState        :: TaxBracketsM a,
                             _trStateCapGain :: a,
                             _trCity         :: TaxBracketsM a
                            } deriving (Show)

data TaxFlow a = TaxFlow { inFlow :: Money a, deductions :: Money a } deriving (Show)

--instance Functor TaxFlow where
--  fmap f (TaxFlow i d) = TaxFlow (f i) (f d)

-- minimal DSL

data TaxEDSL b a where
  GetTaxFlow :: Fractional b => TaxCategory -> (TaxFlow b -> a) -> TaxEDSL b a
  AddDeduction :: Fractional b => TaxCategory -> Money b -> a -> TaxEDSL b a
  GetStatus :: (FilingStatus -> a) -> TaxEDSL b a
  GetRules :: (TaxRulesM b -> a) -> TaxEDSL b a

instance Functor (TaxEDSL b) where
  fmap f (GetTaxFlow c g)     = GetTaxFlow c  (f . g)
  fmap f (AddDeduction c x a) = AddDeduction c x (f a)
  fmap f (GetStatus g)        = GetStatus (f . g)
  fmap f (GetRules g)         = GetRules (f .g)

type TaxComputation b = Free (TaxEDSL b)

getTaxFlow :: Fractional b => TaxCategory -> TaxComputation b (TaxFlow b)
getTaxFlow c = liftF (GetTaxFlow c id)

addDeduction :: Fractional b => TaxCategory -> Money b -> TaxComputation b ()
addDeduction c x = liftF (AddDeduction c x ())

getStatus :: Fractional b => TaxComputation b FilingStatus
getStatus = liftF (GetStatus id)

getRules :: Fractional b => TaxComputation b (TaxRulesM b)
getRules = liftF (GetRules id)

-- Here is an implementation using Reader and State

type FlowState b = Array TaxCategory (TaxFlow b)
data TaxEnv b =  TaxEnv { _teStatus :: FilingStatus, _teRules :: TaxRulesM b }

newtype TaxMonad b a = TaxMonad { unTaxMonad :: ReaderT (TaxEnv b) (State (FlowState b)) a } deriving (Functor, Applicative, Monad, MonadState (FlowState b), MonadReader (TaxEnv b))

runTaxMonad :: TaxEnv b -> FlowState b -> TaxMonad b a -> (a, FlowState b)
runTaxMonad e s tm = runState (runReaderT (unTaxMonad tm) e) s

taxStateProgram :: Fractional b => Free (TaxEDSL b) (Money b) -> TaxMonad b (Money b)

taxStateProgram prog = case prog of
  Free (GetTaxFlow c g) -> do
    taxFlowArray <- get
    let taxFlow = taxFlowArray ! c
    taxStateProgram $ g taxFlow
  Free (AddDeduction c x y) -> do
    taxFlowArray <- get
    let taxFlow = taxFlowArray ! c
        newTaxFlow = (\(TaxFlow i d) -> (TaxFlow i (d <+> x))) taxFlow
        newTaxFlowArray = taxFlowArray // [(c,newTaxFlow)]
    put newTaxFlowArray
    taxStateProgram y
  Free (GetStatus g) -> do
    status <- asks _teStatus
    taxStateProgram $ g status
  Free (GetRules g) -> do
    rules <- asks _teRules
    taxStateProgram (g rules)
  Pure x -> TaxMonad $ return x

--

testFlows :: Fractional b => FlowState b
testFlows = let f = Money in listArray (minBound, maxBound) (repeat (TaxFlow (f 0.0) (f 0.0))) //
  [
    (PayrollIncome, TaxFlow (f 100.0) (f 10.0))
  , (CapitalGain, TaxFlow (f 10) (f 0))
  ]

-- NB: the forall awkwardness here only arises because we have scalar constants.  In a real application we would likely not have those

stateTax :: forall b.Fractional b => TaxComputation b (Money b)
stateTax = do
  payroll <- getTaxFlow PayrollIncome
  capGain <- getTaxFlow CapitalGain
  inheritance <- getTaxFlow Inheritance
  let net (TaxFlow i d) = i <-> d
      tax = ((realToFrac 0.1) :: b) <*> ( net payroll <+> net capGain <+> net inheritance)
  addDeduction PayrollIncome tax
  return tax

fedTax :: forall b.Fractional b => TaxComputation b (Money b)
fedTax = do
  payroll <- getTaxFlow PayrollIncome
  capGain <- getTaxFlow CapitalGain
  inheritance <- getTaxFlow Inheritance
  let net (TaxFlow i d) = i <-> d
      tax = ((fromRational 0.2) :: b) <*> ( net payroll <+> net capGain <+> net inheritance)
  return tax

allTax :: Fractional b => TaxComputation b (Money b)
allTax = do
  state <- stateTax
  fed <- fedTax
  return (state <+> fed)



