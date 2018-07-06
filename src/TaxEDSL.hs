{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TaxEDSL where

import           Control.Monad.Free  (Free (..), liftF)
import           Control.Monad.State (MonadState, State, get, put, runState)
import           Data.Array
import           Data.Ix
import           Data.Map            (Map (..))


--newtype Money = Money Double -- this should not be counted on to be this simple
data TaxCategory = PayrollIncome | OrdinaryIncome | CapitalGain | Dividend | Inheritance | Exempt deriving (Show, Enum, Eq, Bounded, Ord, Ix)

data TaxFlow a = TaxFlow { inFlow :: a, deductions :: a } deriving (Show)

instance Functor TaxFlow where
  fmap f (TaxFlow i d) = TaxFlow (f i) (f d)

data TaxEDSL b a where
  GetTaxFlow :: Num b => TaxCategory -> (TaxFlow b -> a) -> TaxEDSL b a
  AddDeduction :: Num b => TaxCategory -> b -> a -> TaxEDSL b a

instance Functor (TaxEDSL b) where
  fmap f (GetTaxFlow c g)     = GetTaxFlow c  (f . g)
  fmap f (AddDeduction c x a) = AddDeduction c x (f a)

type TaxComputation b = Free (TaxEDSL b)

getTaxFlow :: Num b => TaxCategory -> TaxComputation b (TaxFlow b)
getTaxFlow c = liftF (GetTaxFlow c id)

addDeduction :: Num b => TaxCategory -> b -> TaxComputation b ()
addDeduction c x = liftF (AddDeduction c x ())

type TaxState b = Array TaxCategory (TaxFlow b)

newtype TaxMonad b a = TaxMonad { unTaxMonad :: State (TaxState b) a } deriving (Functor, Applicative, Monad, MonadState (TaxState b))

runTaxMonad :: TaxState b -> TaxMonad b a -> (a, TaxState b)
runTaxMonad s = flip runState s . unTaxMonad

taxStateProgram :: Num b => Free (TaxEDSL b) b -> TaxMonad b b

taxStateProgram prog = case prog of
  Free (GetTaxFlow c g) -> do
    taxFlowArray <- get
    let taxFlow = taxFlowArray ! c
    taxStateProgram $ g taxFlow
  Free (AddDeduction c x y) -> do
    taxFlowArray <- get
    let taxFlow = taxFlowArray ! c
        newTaxFlow = (\(TaxFlow i d) -> (TaxFlow i (d+x))) taxFlow
        newTaxFlowArray = taxFlowArray // [(c,newTaxFlow)]
    put newTaxFlowArray
    taxStateProgram y
  Pure x -> TaxMonad $ return x

testInitial :: TaxState Double
testInitial = listArray (minBound, maxBound) (repeat (TaxFlow 0.0 0.0)) // [(PayrollIncome, TaxFlow 100 10),(CapitalGain, TaxFlow 10 0)]

stateTax :: TaxComputation Double Double
stateTax = do
  payroll <- getTaxFlow PayrollIncome
  capGain <- getTaxFlow CapitalGain
  inheritance <- getTaxFlow Inheritance
  let net (TaxFlow i d) = i - d
      tax = 0.1 * ( net payroll + net capGain + net inheritance)
  addDeduction PayrollIncome tax
  return tax

fedTax :: TaxComputation Double Double
fedTax = do
  payroll <- getTaxFlow PayrollIncome
  capGain <- getTaxFlow CapitalGain
  inheritance <- getTaxFlow Inheritance
  let net (TaxFlow i d) = i - d
      tax = 0.2 * ( net payroll + net capGain + net inheritance)
  return tax

allTax :: TaxComputation Double Double
allTax = do
  state <- stateTax
  fed <- fedTax
  return (state + fed)



