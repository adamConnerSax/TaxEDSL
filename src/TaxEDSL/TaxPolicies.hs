{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TaxEDSL.TaxPolicies
  (
    basePolicy
  ) where

import           TaxEDSL.Core  (BracketType (..), Jurisdiction (..),
                                TaxComputation, TaxFlow (..), TaxFlows (..),
                                TaxType (..), adjustedGrossIncome,
                                applyBrackets, deductions, fedCapGain, flow,
                                grossNonInvestmentIncome, inFlow,
                                medicareSurtax, netFlow, netIncome,
                                netInvestmentIncome, netNonInvestmentIncome,
                                runTaxMonad, saltCap, standardDeduction,
                                sumTaxFlows, taxReaderProgram)
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

saltTaxable :: (Ord b, Fractional b) => TaxComputation b (Money b)
saltTaxable = do
  saltGrossIncome <- sumTaxFlows inFlow [OrdinaryIncome, NonPayrollIncome, CapitalGain, Dividend]
  saltItemizedDeductions <- sumTaxFlows deductions [OrdinaryIncome, NonPayrollIncome, CapitalGain, Dividend]
  saltStandardDeduction <- standardDeduction StateJ -- assuming localities just use their state standard deduction
  let saltDeduction = max saltItemizedDeductions saltStandardDeduction
  return $ max moneyZero (saltGrossIncome |-| saltDeduction)

localTax :: forall b.(Ord b, Fractional b) => TaxComputation b (Money b)
localTax = saltTaxable >>= applyBrackets Local

stateTax :: forall b.(Ord b, Fractional b) => TaxComputation b (Money b)
stateTax = saltTaxable >>= applyBrackets State

allTax :: forall b.(Ord b, Fractional b) => TaxComputation b (Money b, b)
allTax = do
  stateAndLocalTax <- liftM2 (|+|) localTax stateTax

  paidSALT <- (deductions <$> flow StateAndLocal)  -- already paid, usually property taxes
  stateAndLocalDeduction <- do
    let totalSALT = paidSALT |+| stateAndLocalTax
    saltCapM <- saltCap
    return $ maybe totalSALT (min totalSALT) saltCapM

  grossNonInvInc <- sumTaxFlows inFlow [OrdinaryIncome, NonPayrollIncome]
  grossItemizedDeductions <- sumTaxFlows deductions [OrdinaryIncome, NonPayrollIncome]
  standardFedDeduction <- standardDeduction FederalJ
  let fedDeduction = max standardFedDeduction (grossItemizedDeductions |+| stateAndLocalDeduction)
      fedTaxableIncome = max moneyZero (grossNonInvInc |-| fedDeduction)
  fedIncomeTax <- applyBrackets Federal fedTaxableIncome

  netInvInc <- sumTaxFlows netFlow [CapitalGain, Dividend]
  let unusedDeduction = max moneyZero (fedDeduction |-| grossNonInvInc) -- if some deduction went unused we can apply it here
      capGainTaxable = max moneyZero (netInvInc |-| unusedDeduction)
  -- uses special CG brackets and only computes tax on amounts in each bracket above the fedTaxableIncome
  fedCGTax <- fedCapGain fedTaxableIncome capGainTaxable

  payrollTax <- (inFlow <$> flow OrdinaryIncome) >>= applyBrackets Payroll
  estateTax <- (max moneyZero . netFlow <$> flow Inheritance) >>= applyBrackets Estate
  medSurtax <- medicareSurtax
  agi <- adjustedGrossIncome

  let totalNonEstateTax = stateAndLocalTax |+| fedIncomeTax |+| fedCGTax |+| payrollTax |+| medSurtax
      taxRate = if (agi > moneyZero) then totalNonEstateTax |/| agi else (0 :: b)

  return $ (totalNonEstateTax |+| estateTax, taxRate)

basePolicy :: forall b.(Ord b, Fractional b) => TaxComputation b (Money b, b)
basePolicy = allTax

-- Here is an example
