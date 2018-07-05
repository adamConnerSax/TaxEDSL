module TaxEDSL where

import Data.Free
import Control.Monad.State
import Data.Map


--newtype Money = Money Double -- this should not be counted on to be this simple
data TaxCategory = PayrollIncome | OrdinaryIncome | CapitalGain | Dividend | Inheritance | Exempt deriving (Enum, Ord)

data TaxFlow a where
  TaxFlow :: Num a => { inFlow :: a, deductions :: a } -> TaxFlow a

instance Functor TaxFlow where
  fmap f (TaxFlow i d) = TaxFlow (f i) (f d)

data TaxEDSL b a where
  GetTaxFlow :: Num b => TaxCategory -> (TaxFlow b -> a) -> TaxEDSL b a
  AddDeduction :: Num b => TaxCategory -> b -> a -> TaxEDSL b a



instance Functor (TaxEDSL b) where  
  fmap f (GetTaxFlow c g) = GetTaxFlow c  (f . g)
  fmap f d@(AddDeduction _ _) = d

type TaxComputation b = Free (TaxEDSL b) 

getTaxFlow :: Num b => TaxCategory -> TaxComputation b (TaxFlow b)
getTaxFlow c = liftF (GetTaxFlow c id)

addDeduction :: Num b => TaxCategory -> b -> TaxComputation b ()
addDeduction c x = liftF (AddDeduction c x ())

data TaxFlow b = TaxFlow { inFlow :: b, deductions :: b } 

type TaxState b = Map TaxCategory (TaxFlow b)

newtype TaxMonad b a = TaxMonad { unTaxMonad :: State (TaxState b) a }

runTaxMonad :: TaxState b -> TaxMonad b a -> (TaxState b, a)
runTaxMonad = runState
