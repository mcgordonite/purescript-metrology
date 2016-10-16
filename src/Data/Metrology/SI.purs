module Data.Metrology.SI where

import Prelude
import Data.Metrology (class Symbolled, Quantity)


-- | Type representing the SI base unit of length: the "metre".
data MetreUnit
type Metres = Quantity MetreUnit

instance symbolledMetreUnit ∷ Symbolled MetreUnit where
  symbol _ = "m"

-- | Construct a Quantity in metres.
metres ∷ ∀ t. t → Metres t
metres = pure


-- | Type representing the SI base unit of mass: the "kilogram".
data KilogramUnit
type Kilograms = Quantity KilogramUnit

instance symbolledKilogramUnit ∷ Symbolled KilogramUnit where
  symbol _ = "kg"

-- | Construct a Quantity in kilograms.
kilograms ∷ ∀ t. t → Kilograms t
kilograms = pure


-- | Type representing the SI base unit of time: the "second".
data SecondUnit
type Seconds = Quantity SecondUnit

instance symbolledSecondUnit ∷ Symbolled SecondUnit where
  symbol _ = "s"

-- | Construct a Quantity in seconds.
seconds ∷ ∀ t. t → Seconds t
seconds = pure


-- | Type representing the SI base unit of electric current: the "ampere".
data AmpereUnit
type Amperes = Quantity AmpereUnit

instance symbolledAmpereUnit ∷ Symbolled AmpereUnit where
  symbol _ = "A"

-- | Construct a Quantity in amperes.
amperes ∷ ∀ t. t → Amperes t
amperes = pure


-- | Type representing the SI base unit of temperature: the "kelvin".
data KelvinUnit
type Kelvins = Quantity KelvinUnit

instance symbolledKelvinUnit ∷ Symbolled KelvinUnit where
  symbol _ = "K"

-- | Construct a Quantity in kelvins.
kelvins ∷ ∀ t. t → Kelvins t
kelvins = pure


-- | Type representing the SI base unit of substance: the "mole".
data MoleUnit
type Moles = Quantity MoleUnit

instance symbolledMoleUnit ∷ Symbolled MoleUnit where
  symbol _ = "mol"

-- | Construct a Quantity in moles.
moles ∷ ∀ t. t → Moles t
moles = pure


-- | Type representing the SI base unit of luminous intensity: the "candela".
data CandelaUnit
type Candelas = Quantity CandelaUnit

instance symbolledCandelaUnit ∷ Symbolled CandelaUnit where
  symbol _ = "cd"

-- | Construct a Quantity in candelas.
candelas ∷ ∀ t. t → Candelas t
candelas = pure

