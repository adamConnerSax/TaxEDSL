{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           TaxEDSL.Core        (BracketType (..), CapGainBandM (..),
                                      FedCapGainsM (..), Jurisdiction (..),
                                      MedicareSurtaxM (..), TaxBracketM (..),
                                      TaxBracketsM (..), TaxEnv (..),
                                      TaxFlow (..), TaxFlows (..),
                                      TaxRulesM (..), TaxType (..),
                                      capGainBandsToBrackets, runTaxMonad,
                                      taxReaderProgram)
import           TaxEDSL.Money       (Money (..), moneyZero)
import           TaxEDSL.TaxPolicies (basePolicy)

import qualified Data.Array          as A


testFlows :: Fractional b => TaxFlows b
testFlows = let f = Money in A.listArray (minBound, maxBound) (repeat (TaxFlow (f 0.0) (f 0.0))) A.//
  [
    (OrdinaryIncome, TaxFlow (f 1000000.0) (f 0.0))
  , (CapitalGain, TaxFlow (f 25000) (f 0))
  ]

fedBrackets :: Fractional b => TaxBracketsM b
fedBrackets = TaxBracketsM
  [
    BracketM (Money 0) (Money 17850) 0.1
  , BracketM (Money 17850) (Money 72500) 0.15
  , BracketM (Money 72500) (Money 146400) 0.25
  , BracketM (Money 146400) (Money 223050) 0.28
  , BracketM (Money 223050) (Money 398350) 0.33
  , BracketM (Money 398350) (Money 450000) 0.35
  , TopBracketM (Money 450000) 0.396
  ]

stateBrackets :: Fractional b => TaxBracketsM b
stateBrackets = TaxBracketsM
  [
    BracketM (Money 0) (Money 16450) 0.04
  , BracketM (Money 16450) (Money 22600) 0.045
  , BracketM (Money 22600) (Money 26750) 0.0525
  , BracketM (Money 26750) (Money 41150) 0.059
  , BracketM (Money 41150) (Money 154350) 0.0645
  , BracketM (Money 154350) (Money 308750) 0.0665
  , BracketM (Money 308750) (Money 2005850) 0.0685
  , TopBracketM (Money 2005850) 0.0882
  ]

cityBrackets :: Fractional b => TaxBracketsM b
cityBrackets = TaxBracketsM
  [
    BracketM (Money 0) (Money 21600) 0.02907
  , BracketM (Money 21600) (Money 45000) 0.03534
  , BracketM (Money 45000) (Money 90000) 0.03591
  , BracketM (Money 90000) (Money 500000) 0.03648
  , TopBracketM (Money 500000) 0.03876
  ]

payrollBrackets :: Fractional b => TaxBracketsM b
payrollBrackets = TaxBracketsM
  [
    BracketM (Money 0) (Money 117000) 0.062
  , TopBracketM (Money 0) 0.0145 -- medicare
  , TopBracketM (Money 250000) 0.009 -- medicare additional for high income
  ]

testFedCapGainsM :: Fractional b => FedCapGainsM b
testFedCapGainsM = FedCapGainsM 0.2 [CapGainBandM 0.25 0, CapGainBandM 0.39 0.15]

testFedCGBrackets :: (Ord b, Fractional b) => TaxBracketsM b
testFedCGBrackets = capGainBandsToBrackets testFedCapGainsM fedBrackets


testBrackets :: (Ord b, Fractional b) => A.Array BracketType (TaxBracketsM b)
testBrackets = A.array (minBound, maxBound)
  [
    (Federal, fedBrackets)
  , (FedCG, testFedCGBrackets)
  , (State, stateBrackets)
  , (Local, cityBrackets)
  , (Payroll, payrollBrackets)
  , (Estate, TaxBracketsM [])
  ]


testMedSurtax :: Fractional b => MedicareSurtaxM b
testMedSurtax = MedicareSurtaxM 0 moneyZero

testStandardDeductions :: Fractional b => A.Array Jurisdiction (Money b)
testStandardDeductions = A.listArray (minBound, maxBound) (repeat moneyZero) A.// [] -- can add standard deds here

testSALTCap :: Fractional b => Maybe (Money b)
testSALTCap = Nothing

testTaxRules :: (Ord b, Fractional b) => TaxRulesM b
testTaxRules = TaxRulesM testBrackets testStandardDeductions testSALTCap testMedSurtax

test x = runTaxMonad (taxReaderProgram x) (TaxEnv testTaxRules testFlows)

main :: IO ()
main = putStrLn $ "basePolicy: " ++ (show $ test basePolicy)
