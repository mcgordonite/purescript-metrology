module Example.Metrology where

import Prelude
import Control.Comonad (extract)
import Data.Metrology ((.*.), (.+.), (*.))
import Data.Metrology.SI (Seconds, Metres, Kelvins)
import Data.Metrology.SI.Derived (Radians, SquareMetres, MetresPerSecond, MetresPerSquareSecond)


-- The temperature to the nearest Kelvin
temperature :: Kelvins Int
temperature = pure 297

-- Distance travelled at constant acceleration in a given time, starting at a
-- given velocity
distanceTravelled ∷ MetresPerSquareSecond Number → MetresPerSecond Number → Seconds Number → Metres Number
distanceTravelled a u t = u .*. t .+. 0.5 *. a .*. t .*. t

-- The area of a θ radians sector of a circle with radius r metres
sectorArea ∷ Metres Number → Radians Number → SquareMetres Number
sectorArea r θ = (extract θ / (2.0 * pi)) *. r .*. r where
  pi = 3.14159

