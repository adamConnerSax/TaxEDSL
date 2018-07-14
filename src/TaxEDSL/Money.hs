{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
module TaxEDSL.Money
  (
    Money(..)
  , MoneyAddition(..)
  , MoneyMultiplication(..)
  , MoneyDivision(..)
  ) where

data Money a where
  Money :: Fractional a => a -> Money a

instance Show a => Show (Money a) where
  show (Money x) = "Money " ++ show x

instance Eq a => Eq (Money a) where
  (Money x) == (Money y) = (x == y)

instance Ord a => Ord (Money a) where
  compare (Money x) (Money y) = compare x y

type family SumT (a :: *) (b :: *) :: * where
  SumT (Money a) (Money a) = Money a

class (c ~ SumT a b) => MoneyAddition a b c where
  (|+|) :: a -> b -> c
  (|-|) :: a -> b -> c

instance Fractional a => MoneyAddition (Money a) (Money a) (Money a) where
  (Money x) |+| (Money y) = Money (x+y)
  (Money x) |-| (Money y) = Money (x-y)

type family ProdT (a :: *) (b :: *) :: * where
  ProdT a         (Money a) = Money a
  ProdT (Money a) a         = Money a

class (c ~ ProdT a b) => MoneyMultiplication a b c where
  (|*|) :: a -> b -> c

instance Fractional a => MoneyMultiplication a (Money a) (Money a) where
  x |*| (Money y) = Money (x * y)

instance (Fractional a, ProdT (Money a) a ~ (Money a)) => MoneyMultiplication (Money a) a (Money a) where
  (Money x) |*| y = Money (x * y)

type family DivT (a :: *) (b :: *) :: * where
  DivT (Money a) (Money a) = a
  DivT (Money a) a         = Money a

class (c ~ DivT a b) => MoneyDivision a b c where
  (|/|) :: a -> b -> c

instance Fractional a => MoneyDivision (Money a) (Money a) a where
  (Money x) |/| (Money y) = (x/y)

instance (Fractional a, DivT (Money a) a ~ (Money a)) => MoneyDivision (Money a) a (Money a) where
  (Money x) |/| y = Money (x/y)
