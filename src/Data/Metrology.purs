module Data.Metrology where

import Prelude
import Control.Apply (lift2)
import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype)
import Type.Proxy (Proxy(..))


-- | A quantity with a value of type t and a unit of type u.
newtype Quantity u t = Quantity t


derive instance newtypeQuantity ∷ Newtype (Quantity u t) _
derive newtype instance eqQuantity ∷ Eq t ⇒ Eq (Quantity u t)
derive newtype instance ordQuantity ∷ Ord t ⇒ Ord (Quantity u t)
derive newtype instance showQuantity ∷ Show t ⇒ Show (Quantity u t)
derive newtype instance semigroupQuantity ∷ Semigroup t ⇒ Semigroup (Quantity u t)
derive newtype instance monoidQuantity ∷ Monoid t ⇒ Monoid (Quantity u t)


instance functorQuantity ∷ Functor (Quantity u) where
  map = liftM1

instance applyQuantity ∷ Apply (Quantity u) where
  apply = ap

instance applicativeQuantity ∷ Applicative (Quantity u) where
  pure = Quantity

instance bindQuantity ∷ Bind (Quantity u) where
  bind (Quantity a) f = f a

instance monadQuantity ∷ Monad (Quantity u)

instance extendQuantity ∷ Extend (Quantity u) where
  extend f m = Quantity (f m)

instance comonadQuantity ∷ Comonad (Quantity u) where
  extract (Quantity a) = a


-- | Add two quantities together, preserving the unit.
addQ ∷ ∀ u t. Semiring t ⇒ Quantity u t → Quantity u t → Quantity u t
addQ = lift2 add

infixl 6 addQ as .+.

-- | Subtract one quantity from another, preserving the unit
subQ ∷ ∀ u t. Ring t ⇒ Quantity u t → Quantity u t → Quantity u t
subQ = lift2 sub

infixl 6 subQ as .-.

-- | Multiply a quantity by a scalar on the left.
scaleQLeft ∷ ∀ u t. Semiring t ⇒ t → Quantity u t → Quantity u t
scaleQLeft = map <<< mul

infixl 7 scaleQLeft as *.

-- | Multiply a quantity by a scalar on the right.
scaleQRight ∷ ∀ u t. Semiring t ⇒ Quantity u t → t → Quantity u t
scaleQRight q x = map (mul x) q

infixl 7 scaleQRight as .*


-- | Indicates that c is composed of a and b by some multiplication operation.
class ProductUnit a b c | a b → c

-- | Multiply two quantitys' values and units together.
mulQ ∷ ∀ a t. Ring t ⇒ Quantity a t → ∀ b. Quantity b t → ∀ c. ProductUnit a b c ⇒ Quantity c t
mulQ a b = pure (extract a * extract b)

infixl 7 mulQ as .*.

-- | Divide one quantity's value and unit by another's value and unit.
divQ ∷ ∀ c t. EuclideanRing t ⇒ Quantity c t → ∀ b. Quantity b t → ∀ a. ProductUnit a b c ⇒ Quantity a t
divQ a b = pure (extract a / extract b)

infixl 7 divQ as ./.


-- | Class to associate symbols with unit types.
class Symbolled s where
  symbol ∷ Proxy s → String
  
-- | Write out the value and unit symbol for a quantity.
pretty ∷ ∀ u t. (Symbolled u, Show t) ⇒ Quantity u t → String
pretty (Quantity x) = show x <> " " <> symbol (Proxy ∷ Proxy u)

