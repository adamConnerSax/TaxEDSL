module TaxPolicies
  (
    basePolicy
  ) where

import Money (Money(..))
import TaxEDSL (Flow (inflow),
                BracketType(..),
                TaxCategory(..),
                flooredNetIncome,
                grossIncome,
                applyBrackets,
                applyFedCapGain,
                applyMedSurTax,
                flooredNetCGAndDivs,
                stateCapGainRate)                
                
                
                
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
localTax = flooredNetIncome >>= applyBrackets Local

stateTax :: forall b.(Ord b, Fractional b) => TaxComputation b (Money b)
stateTax = do
  stateIncomeTax <- (flooredNetIncome >>= applyBrackets State)
  stateCapGainTaxable <- flooredNetCGAndDivs
  stateCapGainR <- stateCapGainRate
  let stateCapGainTax = stateCapGainR |*| stateCapGainTaxable
  return $ stateIncomeTax |+| stateCapGainTax

allTax :: forall b.(Ord b, Fractional b) => Bool -> TaxComputation b (Money b, b)
allTax deductibleStateAndLocal = do
  let moneyZ = (Money 0) :: Money b

  stateAndLocalTax <- liftM2 (|+|) localTax stateTax
  let stateAndLocalDeduction = if deductibleStateAndLocal then stateAndLocalTax else moneyZ

  fni <- flooredNetIncome
  let fedTaxable = max moneyZ (fni |-| stateAndLocalDeduction)
  fedIncomeTax <- applyBrackets Federal fedTaxable

  fncg <- flooredNetCGAndDivs
  fedCGTax <- applyFedCapGain fedTaxable fncg

  payrollTax <- (inFlow <$> flow OrdinaryIncome) >>= applyBrackets Payroll
  estateTax <- (flooredNetFlow <$> flow Inheritance) >>= applyBrackets Estate
  grossInc <- grossIncome

  let totalNITax = stateAndLocalTax |+| fedIncomeTax |+| fedCGTax |+| payrollTax
      totalNITaxable = fncg |+| grossInc
      taxRate = if (totalNITaxable > moneyZ) then totalNITax |/| totalNITaxable else (0 :: b)

  return $ (totalNITax |+| estateTax, taxRate)

basePolicy :: forall b.(Ord b, Fractional b) => Bool -> TaxComputation b (Money b, b)
basePolicy = allTax

-- Here is an example
