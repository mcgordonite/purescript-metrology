module Data.Metrology.SI.Derived where

import Prelude
import Data.Metrology (class ProductUnit, class Symbolled, Quantity)
import Data.Metrology.SI (KilogramUnit, MetreUnit, SecondUnit)


-- | Type representing the SI derived unit "radians".
data RadianUnit
type Radians = Quantity RadianUnit

instance symbolledRadianUnit ∷ Symbolled RadianUnit where
  symbol _ = "radians"

-- | Construct a Quantity in radians.
radians ∷ ∀ t. t → Radians t
radians = pure


-- | Type representing the SI derived unit "sqare metres".
data SquareMetreUnit
type SquareMetres = Quantity SquareMetreUnit

instance symbolledSquareMetreUnit ∷ Symbolled SquareMetreUnit where
  symbol _ = "m^2"

instance squareMetreProductUnit ∷ ProductUnit MetreUnit MetreUnit SquareMetreUnit

-- | Construct a Quantity in square metres.
squareMetres ∷ ∀ t. t → SquareMetres t
squareMetres = pure


-- | Type representing the SI derived unit "square seconds".
data SquareSecondUnit
type SquareSeconds = Quantity SquareSecondUnit

instance symbolledSquareSecondUnit ∷ Symbolled SquareSecondUnit where
  symbol _ = "s^2"

instance squareSecondProductUnit ∷ ProductUnit SecondUnit SecondUnit SquareSecondUnit

-- | Construct a Quantity in square seconds.
squareSeconds ∷ ∀ t. t → SquareSeconds t
squareSeconds = pure


-- | Type representing the SI derived unit "radians per second".
data RadianPerSecondUnit
type RadiansPerSecond = Quantity RadianPerSecondUnit

instance symbolledRadianPerSecondUnit ∷ Symbolled RadianPerSecondUnit where
  symbol _ = "radians/s"

instance radianPerSecondProductUnit ∷ ProductUnit RadianPerSecondUnit SecondUnit MetreUnit
instance inverseRadianPerSecondProductUnit ∷ ProductUnit SecondUnit RadianPerSecondUnit MetreUnit

-- | Construct a Quantity in radians per second.
radiansPerSecond ∷ ∀ t. t → RadiansPerSecond t
radiansPerSecond = pure


-- | Type representing the SI derived unit metres per second.
data MetrePerSecondUnit
type MetresPerSecond = Quantity MetrePerSecondUnit

instance symbolledMetrePerSecondUnit ∷ Symbolled MetrePerSecondUnit where
  symbol _ = "m/s"

instance metrePerSecondProductUnit ∷ ProductUnit MetrePerSecondUnit SecondUnit MetreUnit
instance inverseMetrePerSecondProductUnit ∷ ProductUnit SecondUnit MetrePerSecondUnit MetreUnit

-- | Construct a Quantity in metres per second.
metresPerSecond ∷ ∀ t. t → MetresPerSecond t
metresPerSecond = pure


-- | Type representing the SI derived unit "metres per square second".
data MetrePerSquareSecondUnit
type MetresPerSquareSecond = Quantity MetrePerSquareSecondUnit

instance symbolledMetrePerSquareSecondUnit ∷ Symbolled MetrePerSquareSecondUnit where
  symbol _ = "m/s/s"

instance metrePerSquareSecondSquareSecondProductUnit ∷ ProductUnit SquareSecondUnit MetrePerSquareSecondUnit MetreUnit
instance inverseMetrePerSquareSecondSquareSecondProductUnit ∷ ProductUnit MetrePerSquareSecondUnit SquareSecondUnit MetreUnit

instance metrePerSquareSecondSecondProductUnit ∷ ProductUnit SecondUnit MetrePerSquareSecondUnit MetrePerSecondUnit
instance inverseMetrePerSquareSecondSecondProductUnit ∷ ProductUnit MetrePerSquareSecondUnit SecondUnit MetrePerSecondUnit

-- | Construct a Quantity in metres per square second.
metresPerSquareSecond ∷ ∀ t. t → MetresPerSquareSecond t
metresPerSquareSecond = pure


-- | Type representing the SI derived unit "Newtons".
data NewtonUnit
type Newtons = Quantity NewtonUnit

instance symbolledNewtonUnit ∷ Symbolled NewtonUnit where
  symbol _ = "N"

instance kilogramMetrePerSquareSecondProductUnit ∷ ProductUnit KilogramUnit MetrePerSquareSecondUnit NewtonUnit
instance inverseKilogramMetrePerSquareSecondProductUnit ∷ ProductUnit MetrePerSquareSecondUnit KilogramUnit NewtonUnit

newtons ∷ ∀ t. t → Newtons t
newtons = pure
