{-# LANGUAGE GADTs #-}
module TaxEDSL where

import Control.Monad.Free (Free(..), liftF)
import Control.Monad.State (State, runState, get, put)
import Data.Map (Map(..))
import Data.Ix
import Data.Array


--newtype Money = Money Double -- this should not be counted on to be this simple
data TaxCategory = PayrollIncome | OrdinaryIncome | CapitalGain | Dividend | Inheritance | Exempt deriving (Enum, Eq, Bounded, Ord, Ix)

data TaxFlow a where
  TaxFlow :: Num a => { inFlow :: a, deductions :: a } -> TaxFlow a

instance Functor TaxFlow where
  fmap f (TaxFlow i d) = TaxFlow (f i) (f d)

data TaxEDSL b a where
  GetTaxFlow :: Num b => TaxCategory -> (TaxFlow b -> a) -> TaxEDSL b a
  AddDeduction :: Num b => TaxCategory -> b -> a -> TaxEDSL b a

instance Functor (TaxEDSL b) where  
  fmap f (GetTaxFlow c g) = GetTaxFlow c  (f . g)
  fmap f (AddDeduction c x a) = AddDeduction c x (f a)

type TaxComputation b = Free (TaxEDSL b) 

getTaxFlow :: Num b => TaxCategory -> TaxComputation b (TaxFlow b)
getTaxFlow c = liftF (GetTaxFlow c id)

addDeduction :: Num b => TaxCategory -> b -> TaxComputation b ()
addDeduction c x = liftF (AddDeduction c x ())

type TaxState b = Array TaxCategory (TaxFlow b)

newtype TaxMonad b a = TaxMonad { unTaxMonad :: State (TaxState b) a }

runTaxMonad :: TaxState b -> TaxMonad b a -> (TaxState b, a)
runTaxMonad = runState . unTaxMonad

taxStateProgram :: Num b => Free (TaxEDSL b) b -> TaxMonad b b

taxStateProgram prog = TaxMonad $ case prog of
  Free (GetTaxFlow c g) -> do
    taxFlowArray <- get
    let taxFlow = taxFlowArray ! c
    return $ g taxFlow
  Free (AddDeduction c x y) -> do
    taxFlowArray <- get
    let taxFlow = taxFlowArray ! c
        newTaxFlow = (\(TaxFlow i d) -> (TaxFlow i (d+x))) taxFlow
        newTaxFlowArray = taxFlowArray // [(c,newTaxFlow)]
    put newTaxFlowArray
    return y
  Pure x -> return x

testInitial :: TaxState Double
testInitial = listArray (minBound, maxBound) [0..] // [(PayrollIncome, TaxFlow 100 10),(CapitalGain, TaxFlow 10 0)]

stateTax :: TaxMonad Double Double
stateTax = do
  payroll <- getTaxFlow PayrollIncome
  capGain <- getTaxFlow CapitalGain
  inheritance <- getTaxFlow Inheritance
  let net (TaxFlow i d) = i - d
      tax = 0.1 * ( net payroll + net capGain + net inheritance)
  addDeduction PayrollIncome tax
  return tax

fedTax :: TaxMonad Double Double  
fedTax = do
  payroll <- getTaxFlow PayrollIncome
  capGain <- getTaxFlow CapitalGain
  inheritance <- getTaxFlow Inheritance
  let net (TaxFlow i d) = i - d
      tax = 0.2 * ( net payroll + net capGain + net inheritance)
  return tax

allTax :: TaxMonad Double Double
allTax = do
  state <- stateTax
  fed <- fedTax
  return (state + fed)
  
  
  
