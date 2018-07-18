{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           TaxEDSL.Core        (BracketType (..), CapGainBandM (..),
                                      FedCapitalGainsM (..),
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
  , BracketM (Money 0) (Money 250000) 0.0145
  , TopBracketM (Money 250000) 0.009
  ]

testBrackets :: forall b.Fractional b => A.Array BracketType (TaxBracketsM b)
testBrackets = let moneyZ = (Money 0) :: Money b in A.array (minBound, maxBound)
  [
    (Federal, fedBrackets)
  , (State, stateBrackets)
  , (Local, cityBrackets)
  , (Payroll, payrollBrackets)
  , (Estate, TaxBracketsM [])
  ]

testFedCapGainsM :: Fractional b => FedCapitalGainsM b
testFedCapGainsM = FedCapitalGainsM 0.2 [CapGainBandM 0.25 0, CapGainBandM 0.39 0.15]

testMedSurtax :: forall b.Fractional b => MedicareSurtaxM b
testMedSurtax = let moneyZ = (Money 0) :: Money b in MedicareSurtaxM 0 0 moneyZ

testTaxRules :: Fractional b => TaxRulesM b
testTaxRules = TaxRulesM testBrackets testFedCapGainsM testMedSurtax 0.09

test x = runTaxMonad (taxReaderProgram x) (TaxEnv testTaxRules testFlows)

main :: IO ()
main = putStrLn $ "basePolicy: " ++ (show $ test $ basePolicy True)
