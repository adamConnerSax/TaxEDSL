{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           TaxEDSL.Core        (BracketType (..), FedCapitalGainsM (..),
                                      MedicareSurtaxM (..), TaxBracketM (..),
                                      TaxBracketsM (..), TaxEnv (..),
                                      TaxFlow (..), TaxFlows (..),
                                      TaxRulesM (..), TaxType (..), runTaxMonad,
                                      taxReaderProgram)
import           TaxEDSL.Money       (Money (..))
import           TaxEDSL.TaxPolicies (basePolicy)

import qualified Data.Array          as A


testFlows :: Fractional b => TaxFlows b
testFlows = let f = Money in A.listArray (minBound, maxBound) (repeat (TaxFlow (f 0.0) (f 0.0))) A.//
  [
    (OrdinaryIncome, TaxFlow (f 100.0) (f 0.0))
  , (CapitalGain, TaxFlow (f 0) (f 0))
  ]

testBrackets :: forall b.Fractional b => A.Array BracketType (TaxBracketsM b)
testBrackets = let moneyZ = (Money 0) :: Money b in A.array (minBound, maxBound)
  [
    (Federal, TaxBracketsM [TopBracketM moneyZ 0.2])
  , (State, TaxBracketsM [TopBracketM moneyZ 0.1])
  , (Local, TaxBracketsM [])
  , (Payroll, TaxBracketsM [])
  , (Estate, TaxBracketsM [])
  ]

testFedCapGainsM :: Fractional b => FedCapitalGainsM b
testFedCapGainsM = FedCapitalGainsM 0.0 []

testMedSurtax :: forall b.Fractional b => MedicareSurtaxM b
testMedSurtax = let moneyZ = (Money 0) :: Money b in MedicareSurtaxM 0 0 moneyZ

testTaxRules :: Fractional b => TaxRulesM b
testTaxRules = TaxRulesM testBrackets testFedCapGainsM testMedSurtax 0

test x = runTaxMonad (taxReaderProgram x) (TaxEnv testTaxRules testFlows)

main :: IO ()
main = putStrLn $ "basePolicy: " ++ (show $ test $ basePolicy True)
