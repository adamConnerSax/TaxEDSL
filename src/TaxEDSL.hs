{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
module TaxEDSL where

import           Control.Monad.Free  (Free (..), liftF)
import           Control.Monad.State (MonadState, State, get, put, runState)
import           Data.Array
import           Data.Ix
import           Data.Map            (Map (..))

import           Prelude             hiding ((<*>))

--newtype Money = Money Double -- this should not be counted on to be this simple
data TaxCategory = PayrollIncome | OrdinaryIncome | CapitalGain | Dividend | Inheritance | Exempt deriving (Show, Enum, Eq, Bounded, Ord, Ix)

data Money a where
  Money :: Fractional a => a -> Money a

instance Show a => Show (Money a) where
  show (Money x) = "Money " ++ show x

type family SumT (a :: *) (b :: *) :: * where
  SumT (Money a) (Money a) = Money a

class (c ~ SumT a b) => HasAddition a b c where
  (<+>) :: a -> b -> c
  (<->) :: a -> b -> c

instance Fractional a => HasAddition (Money a) (Money a) (Money a) where
  (Money x) <+> (Money y) = Money (x+y)
  (Money x) <-> (Money y) = Money (x-y)

type family ProdT (a :: *) (b :: *) :: * where
  ProdT a         (Money a) = Money a
  ProdT (Money a) a         = Money a

class (c ~ ProdT a b) => HasMultiplication a b c where
  (<*>) :: a -> b -> c

instance Fractional a => HasMultiplication a (Money a) (Money a) where
  x <*> (Money y) = Money (x * y)

instance (Fractional a, ProdT (Money a) a ~ (Money a)) => HasMultiplication (Money a) a (Money a) where
  (Money x) <*> y = Money (x * y)

type family DivT (a :: *) (b :: *) :: * where
  DivT (Money a) (Money a) = a
  DivT (Money a) a         = Money a

class (c ~ DivT a b) => HasDivision a b c where
  (</>) :: a -> b -> c

instance Fractional a => HasDivision (Money a) (Money a) a where
  (Money x) </> (Money y) = (x/y)

instance (Fractional a, DivT (Money a) a ~ (Money a)) => HasDivision (Money a) a (Money a) where
  (Money x) </> y = Money (x/y)

data TaxFlow a = TaxFlow { inFlow :: Money a, deductions :: Money a } deriving (Show)

--instance Functor TaxFlow where
--  fmap f (TaxFlow i d) = TaxFlow (f i) (f d)

data TaxEDSL b a where
  GetTaxFlow :: Fractional b => TaxCategory -> (TaxFlow b -> a) -> TaxEDSL b a
  AddDeduction :: Fractional b => TaxCategory -> Money b -> a -> TaxEDSL b a

instance Functor (TaxEDSL b) where
  fmap f (GetTaxFlow c g)     = GetTaxFlow c  (f . g)
  fmap f (AddDeduction c x a) = AddDeduction c x (f a)

type TaxComputation b = Free (TaxEDSL b)

getTaxFlow :: Fractional b => TaxCategory -> TaxComputation b (TaxFlow b)
getTaxFlow c = liftF (GetTaxFlow c id)

addDeduction :: Fractional b => TaxCategory -> Money b -> TaxComputation b ()
addDeduction c x = liftF (AddDeduction c x ())

type TaxState b = Array TaxCategory (TaxFlow b)

newtype TaxMonad b a = TaxMonad { unTaxMonad :: State (TaxState b) a } deriving (Functor, Applicative, Monad, MonadState (TaxState b))

runTaxMonad :: TaxState b -> TaxMonad b a -> (a, TaxState b)
runTaxMonad s = flip runState s . unTaxMonad

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
  Pure x -> TaxMonad $ return x

testInitial :: Fractional b => TaxState b
testInitial = let f = Money in listArray (minBound, maxBound) (repeat (TaxFlow (f 0.0) (f 0.0))) //
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



