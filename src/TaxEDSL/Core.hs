{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
module TaxEDSL.Core
  (
    TaxFlow(..)
  , TaxType(..)
  , BracketType(..)
  , Jurisdiction(..)
  , TaxBracketsM(..)
  , TaxBracketM(..)
  , FedCapGainsM(..)
  , CapGainBandM(..)
  , MedicareSurtaxM(..)
  , TaxRulesM(..)
  , TaxComputation
  , TaxFlows
  , TaxEnv(..)
  , netFlow
  , sumTaxFlows
  , flooredNetFlow
  , netIncome
  , adjustedGrossIncome
  , modifiedAGI
  , netNonInvestmentIncome
  , grossNonInvestmentIncome
  , netInvestmentIncome
  , grossInvestmentIncome
  , flow
  , brackets
  , standardDeduction
  , saltCap
  , medSurtaxInfo
  , capGainBandsToBrackets
  , applyBrackets
  , fedCapGain
  , medicareSurtax
  , taxReaderProgram
  , runTaxMonad
  )  where

import           TaxEDSL.Money

import           Control.Monad.Free   (Free (..), liftF)
import           Control.Monad.Reader (MonadReader, Reader, asks, runReader)
import qualified Data.Array           as A
import           Data.Ix              (Ix)

import           Data.Foldable        (foldl')
import           Prelude              hiding ((<*>))

-- These will come from here
data TaxType = NonPayrollIncome | OrdinaryIncome | CapitalGain | Dividend | ExemptInterest | Inheritance | StateAndLocal deriving (Show, Enum, Eq, Bounded, Ord, Ix)
data BracketType = Federal | FedCG | State | Local | Payroll | Estate deriving (Show, Bounded, Eq, Ord, Enum, Ix) -- Payroll does Social Security and Medicare taxes
data Jurisdiction = FederalJ | StateJ deriving (Show, Bounded, Eq, Ord, Enum, Ix)


-- These will be exported so things can be converted to them
data CapGainBandM a = CapGainBandM { marginalRateM :: a, capGainRateM :: a } deriving (Show) -- just for conversion to CG bracket
data FedCapGainsM a = FedCapGainsM { topRate :: a, bands :: [CapGainBandM a] } -- just for conversion to CG bracket

data TaxBracketM a = BracketM (Money a) (Money a) a | TopBracketM (Money a) a deriving (Show)
data TaxBracketsM a = TaxBracketsM ![TaxBracketM a] deriving (Show) -- we don't expose this constructor
data MedicareSurtaxM a = MedicareSurtaxM { netInvRateM :: a, magiThresholdM :: Money a } deriving (Show) -- payroll piece included in payroll bracket

data TaxRulesM a = TaxRulesM { _trBrackets           :: A.Array BracketType (TaxBracketsM a),
                               _trStandardDeductions :: A.Array Jurisdiction (Money a),
                               _trSALTCap            :: Maybe (Money a),
                               _trMedSurtax          :: MedicareSurtaxM a
                             } deriving (Show)

data TaxFlow a = TaxFlow { inFlow :: Money a, deductions :: Money a } deriving (Show)

netFlow :: Fractional a => TaxFlow a -> Money a
netFlow (TaxFlow i d) = i |-| d

flooredNetFlow :: (Ord a, Fractional a) => TaxFlow a -> Money a
flooredNetFlow tf = max moneyZero (netFlow tf)

--instance Functor TaxFlow where
--  fmap f (TaxFlow i d) = TaxFlow (f i) (f d)

-- DSL
data TaxEDSL b a where
  Flow              :: Fractional b => TaxType -> (TaxFlow b -> a) -> TaxEDSL b a -- get current flow details in bucket
  MedSurtaxInfo     :: Fractional b => (MedicareSurtaxM b -> a) -> TaxEDSL b a -- compute the medicare surtax (payroll and inv income) from MAGI and inv invcome
  Brackets          :: Fractional b => BracketType -> (TaxBracketsM b -> a) -> TaxEDSL b a -- get bracket.  We will not export access to this
  StandardDeduction :: Fractional b => Jurisdiction -> (Money b -> a) -> TaxEDSL b a -- get standard deduction
  SALTCap           :: Fractional b => (Maybe (Money b) -> a) -> TaxEDSL b a

instance Functor (TaxEDSL b) where
  fmap f (Flow c g)              = Flow c  (f . g)
  fmap f (MedSurtaxInfo g)       = MedSurtaxInfo (f . g)
  fmap f (Brackets bt g)         = Brackets bt (f . g)
  fmap f (StandardDeduction j g) = StandardDeduction j (f . g)
  fmap f (SALTCap g)             = SALTCap (f . g)

type TaxComputation b = Free (TaxEDSL b)

-- basics
flow :: Fractional b => TaxType -> TaxComputation b (TaxFlow b)
flow c = liftF (Flow c id)

medSurtaxInfo :: Fractional b => TaxComputation b (MedicareSurtaxM b)
medSurtaxInfo = liftF (MedSurtaxInfo id)

brackets :: Fractional b => BracketType -> TaxComputation b (TaxBracketsM b)
brackets bt = liftF (Brackets bt id)

standardDeduction :: Fractional b => Jurisdiction -> TaxComputation b (Money b)
standardDeduction j = liftF (StandardDeduction j id)

saltCap :: Fractional b => TaxComputation b (Maybe (Money b))
saltCap = liftF (SALTCap id)

-- utilities
capGainBandsToBrackets :: (Fractional b, Ord b) => FedCapGainsM b -> TaxBracketsM b -> TaxBracketsM b
capGainBandsToBrackets (FedCapGainsM topR bands) (TaxBracketsM brackets) =
  let g bktRate x (CapGainBandM mr cgr) = if (bktRate <= mr) && (cgr < x) then cgr else x
      f bktRate = foldl' (g bktRate) topR bands
      newBracket (BracketM b t r)  = BracketM b t (f r)
      newBracket (TopBracketM b r) = TopBracketM b (f r)
  in TaxBracketsM (newBracket <$> brackets)

-- helpers
-- specialize a fold for combining tax flows
onTaxFlows :: Fractional b => (TaxFlow b -> c) -> (c -> c -> c) -> c -> [TaxType] -> TaxComputation b c
onTaxFlows fEach combine initial types = foldl' combine initial <$> (sequence . fmap (fmap fEach . flow) $ types)

-- specialize further to the case of (|+|) starting at 0
sumTaxFlows :: Fractional b => (TaxFlow b -> Money b) -> [TaxType] -> TaxComputation b (Money b)
sumTaxFlows toMoney types = onTaxFlows toMoney (|+|) moneyZero types

netIncome :: (Ord b, Fractional b) => TaxComputation b (Money b)
netIncome = max moneyZero <$> sumTaxFlows netFlow [OrdinaryIncome, NonPayrollIncome, CapitalGain, Dividend]

adjustedGrossIncome :: Fractional b => TaxComputation b (Money b)
adjustedGrossIncome = sumTaxFlows inFlow [OrdinaryIncome, NonPayrollIncome, CapitalGain, Dividend]

modifiedAGI :: Fractional b => TaxComputation b (Money b)
modifiedAGI = sumTaxFlows inFlow [OrdinaryIncome, NonPayrollIncome, CapitalGain, Dividend, ExemptInterest]

netNonInvestmentIncome :: (Ord b, Fractional b) => TaxComputation b (Money b)
netNonInvestmentIncome = max moneyZero <$> sumTaxFlows netFlow [OrdinaryIncome, NonPayrollIncome]

grossNonInvestmentIncome :: (Ord b, Fractional b) => TaxComputation b (Money b)
grossNonInvestmentIncome = sumTaxFlows inFlow [OrdinaryIncome, NonPayrollIncome]

grossInvestmentIncome :: (Ord b, Fractional b) => TaxComputation b (Money b)
grossInvestmentIncome = sumTaxFlows inFlow [CapitalGain, Dividend, ExemptInterest]

netInvestmentIncome :: (Ord b, Fractional b) => TaxComputation b (Money b)
netInvestmentIncome = max moneyZero <$> sumTaxFlows netFlow [CapitalGain, Dividend]


-- compute tax from income but allow an offset of untaxed income which pushes up brackets but is not taxed
-- this will be 0 for ordinary bracket use but for a cap gain calculation, income will be sum of non-investment and cap gain
-- with the non-investment used as an offset since it's already been taxed at income tax rates
applyBracketsGeneral :: (Ord b, Fractional b) => BracketType -> Money b -> Money b -> TaxComputation b (Money b)
applyBracketsGeneral bt offset inc = do
  (TaxBracketsM bkts) <- brackets bt
  let amtIn x (BracketM bot top _) = min (max moneyZero (x |-| bot)) (top |-| bot)
      amtIn x (TopBracketM bot _)  = max moneyZero (x |-| bot)
      taxFromBracket :: (Ord b, Fractional b) => Money b -> Money b -> TaxBracketM b -> Money b
      taxFromBracket x y bt@(BracketM _ _ r) = let (xIn, yIn) = (amtIn x bt, amtIn y bt) in r |*| (yIn |-| xIn)
      taxFromBracket x y bt@(TopBracketM _ r) = let (xIn, yIn) = (amtIn x bt, amtIn y bt) in r |*| (yIn |-| xIn)
      go :: (Ord b, Fractional b) => Money b -> Money b -> [TaxBracketM b] -> Money b
      go x y bs = foldl' (\tot bkt -> tot |+| (taxFromBracket x y bkt)) moneyZero bs
  return $ go offset inc bkts

applyBrackets :: (Ord b, Fractional b) => BracketType -> Money b -> TaxComputation b (Money b) -- forall here for scopedTypeVars needed for (Money 0)
applyBrackets bt inc = applyBracketsGeneral bt moneyZero inc

fedCapGain :: (Ord b, Fractional b) => Money b -> Money b -> TaxComputation b (Money b)
fedCapGain netNonInv netInv = applyBracketsGeneral FedCG netNonInv (netNonInv |+| netInv)

medicareSurtax :: forall b. (Fractional b, Ord b) => TaxComputation b (Money b)
medicareSurtax = do
  let moneyZ = (Money 0) :: Money b
  (MedicareSurtaxM rate magiThreshold) <- medSurtaxInfo
  magi <- modifiedAGI
  invInc <- netInvestmentIncome
  let medSTaxable = min invInc (max moneyZ (magi |-| magiThreshold))
  return $ rate |*| medSTaxable

-- Here is one possible implementation and interpreter using Reader
-- these could also be hard-wired or come from a DB or a SOAP query or a form
type TaxFlows b = A.Array TaxType (TaxFlow b)
data TaxEnv b =  TaxEnv { _teRules :: TaxRulesM b, _teFlows :: TaxFlows b }

newtype TaxMonad b a = TaxMonad { unTaxMonad :: Reader (TaxEnv b) a } deriving (Functor, Applicative, Monad, MonadReader (TaxEnv b))

runTaxMonad :: TaxMonad b a -> TaxEnv b -> a
runTaxMonad tm = runReader (unTaxMonad tm)

taxReaderProgram :: Fractional b => Free (TaxEDSL b) a -> TaxMonad b a
taxReaderProgram prog = case prog of
  Free (Flow c g) -> do
    taxFlowArray <- asks _teFlows
    taxReaderProgram . g $ taxFlowArray A.! c
  Free (MedSurtaxInfo g) -> do
    mst <- asks (_trMedSurtax . _teRules)
    taxReaderProgram $ g mst -- unimplemented for now
  Free (Brackets bt g) -> do
    bkts <- asks (_trBrackets . _teRules)
    taxReaderProgram . g $ bkts A.! bt
  Free (StandardDeduction j g) -> do
    deds <- asks (_trStandardDeductions . _teRules)
    taxReaderProgram . g $ deds A.! j
  Free (SALTCap g) -> do
    cap <- asks (_trSALTCap . _teRules)
    taxReaderProgram $ g cap
  Pure x -> TaxMonad $ return x






