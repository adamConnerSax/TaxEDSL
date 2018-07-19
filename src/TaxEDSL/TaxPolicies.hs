{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TaxEDSL.TaxPolicies
  (
    basePolicy
  ) where

import           TaxEDSL.Core  (BracketType (..), TaxComputation, TaxFlow (..),
                                TaxFlows (..), TaxType (..),
                                adjustedGrossIncome, applyBrackets, fedCapGain,
                                flooredNetFlow, flooredNetInvestmentIncome,
                                flooredNetNonInvestmentIncome, flow,
                                grossNonInvestmentIncome, inFlow,
                                medicareSurtax, runTaxMonad, saltCap,
                                stateCapGainRate, taxReaderProgram)
import           TaxEDSL.Money (Money (..), MoneyAddition (..),
                                MoneyDivision (..), MoneyMultiplication (..),
                                moneyZero)

import           Control.Monad (liftM2)


{-
This implementation matches what's in FinanciaMC plus (NOT YET) the medicare surtax on investment income.
It does not account for AMT or the possibility of taking standard deductions.
TODO
1. Add Medicare surtax on investment income (easy)
2. Add standard deductions and choose correctly between itemized and standard (requires adding standard deduction data to inputs)
3. Add AMT and use it when required/better (will AMT require some "memory"?  Will we need to know that we did AMT in the previous year?).  This
is lower priority since the disallowing of state & local makes this much less likely
-}

localTax :: forall b.(Ord b, Fractional b) => TaxComputation b (Money b)
localTax = flooredNetNonInvestmentIncome >>= applyBrackets Local

stateTax :: forall b.(Ord b, Fractional b) => TaxComputation b (Money b)
stateTax = do
  stateIncomeTax <- (flooredNetNonInvestmentIncome >>= applyBrackets State)
  stateCapGainTax <- liftM2 (|*|) stateCapGainRate flooredNetInvestmentIncome
  return $ stateIncomeTax |+| stateCapGainTax

allTax :: forall b.(Ord b, Fractional b) => TaxComputation b (Money b, b)
allTax = do
  stateAndLocalTax <- liftM2 (|+|) localTax stateTax

  paidSALT <- (deductions <$> flow StateAndLocal)  -- already paid, usually property taxes
  stateAndLocalDeduction <- do
    let totalSALT = paidSALT |+| stateAndLocalTax
    saltCapM <- saltCap
    return $ maybe totalSALT (min totalSALT) saltCapM

  fni <- flooredNetNonInvestmentIncome
  let fedTaxable = max moneyZero (fni |-| stateAndLocalDeduction)
  fedIncomeTax <- applyBrackets Federal fedTaxable
  fedCGTax <- fedCapGain

  payrollTax <- (inFlow <$> flow OrdinaryIncome) >>= applyBrackets Payroll
  estateTax <- (flooredNetFlow <$> flow Inheritance) >>= applyBrackets Estate
  medSurtax <- medicareSurtax
  agi <- adjustedGrossIncome

  let totalNonEstateTax = stateAndLocalTax |+| fedIncomeTax |+| fedCGTax |+| payrollTax |+| medSurtax
      taxRate = if (agi > moneyZero) then totalNonEstateTax |/| agi else (0 :: b)

  return $ (totalNonEstateTax |+| estateTax, taxRate)

basePolicy :: forall b.(Ord b, Fractional b) => TaxComputation b (Money b, b)
basePolicy = allTax

-- Here is an example
