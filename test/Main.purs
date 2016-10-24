module Test.Main where

import Prelude
import Test.Unit.Assert as Assert
import Control.Comonad ((<<=), extract, extend)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Data.Metrology (class ProductUnit, class Symbolled, Quantity(..), pretty, (.+.), (.-.), (*.), (.*), (.*.), (./.))
import Data.Monoid (mempty)
import Test.QuickCheck ((===))
import Test.Unit (suite, test)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)


data KittenUnit
type Kittens = Quantity KittenUnit

instance symbolledKittenUnit ∷ Symbolled KittenUnit where
  symbol _ = "kittens"

data MinuteUnit
type Minutes = Quantity MinuteUnit

instance symbolledMinuteUnit ∷ Symbolled MinuteUnit where
  symbol _ = "minutes"

data KittenPerMinuteUnit
type KittensPerMinute = Quantity KittenPerMinuteUnit

instance symbolledKittenPerMinuteUnit ∷ Symbolled KittenPerMinuteUnit where
  symbol _ = "kittens/minute"

instance productUnitKittenPerMinute ∷ ProductUnit MinuteUnit KittenPerMinuteUnit KittenUnit
instance inverseProductUnitKittenPerMinute ∷ ProductUnit KittenPerMinuteUnit MinuteUnit KittenUnit

kittens ∷ ∀ t. t → Kittens t
kittens = pure

intKittens ∷ Int → Kittens Int
intKittens = pure

stringKittens ∷ String → Kittens String
stringKittens = pure

minutes ∷ ∀ t. t → Minutes t
minutes = pure

intMinutes ∷ Int → Minutes Int
intMinutes = pure

kittensPerMinute ∷ ∀ t. t → KittensPerMinute t
kittensPerMinute = pure

intKittensPerMinute ∷ Int → KittensPerMinute Int
intKittensPerMinute = pure


main ∷ Eff (avar ∷ AVAR, console ∷ CONSOLE, random ∷ RANDOM, testOutput ∷ TESTOUTPUT) Unit
main = runTest do
  suite "Quantity" do
    test "Eq" do
      Assert.assert "3s == 3s" (kittens 3 == kittens 3)
      Assert.assert "3s /= 4s" (kittens 3 /= kittens 4)
    test "Ord" do
      Assert.assert "1s < 2s" (kittens 1 < kittens 2)
      Assert.assert "2s > 1" (kittens 2 > kittens 1)
      Assert.assert "compare 1s 1s == EQ" (compare (kittens 1) (kittens 1) == EQ)
    test "Functor" do
      Assert.equal (kittens 3) (map ((+) 1) (kittens 2))
      quickCheck \x → id <$> intKittens x === intKittens x
      quickCheck \x → (+) 1 <<< (*) 2 <$> intKittens x === (+) 1 <$> (*) 2 <$> intKittens x
    test "Apply" do
      Assert.equal(kittens 3) (kittens ((+) 2) <*> kittens 1) 
      quickCheck \x → (<<<) <$> kittens ((+) 2) <*> kittens ((*) 3) <*> intKittens x === kittens ((+) 2) <*> (kittens ((*) 3) <*> intKittens x)
    test "Applicative" do
      Assert.equal (intKittens 2) (pure 2 ∷ Kittens Int)
      quickCheck \x → pure id <*> intKittens x === intKittens x
      quickCheck \x → pure (<<<) <*> kittens ((-) 1) <*> kittens ((+) 3) <*> intKittens x === kittens ((-) 1) <*> (kittens ((+) 3) <*> intKittens x)
      quickCheck \x → pure ((+) 5) <*> pure x === (pure (5 + x) ∷ Kittens Int)
    test "Bind" do
      Assert.equal(intKittens 5) (intKittens 3 >>= kittens <<< (+) 2) 
    test "Monad" do
      quickCheck \x → (pure x >>= kittens <<< ((+) 2)) === (kittens (2 + x))
      quickCheck \x → (kittens x >>= pure) === intKittens x
    test "Extend" do
      Assert.equal (intKittens 6) (extend (\(Quantity x) → 2 * x) (intKittens 3))
      quickCheck \x → 
        let 
          f = \(Quantity i) → 2 * i
          g = \(Quantity i) → 1 + i
          q = intKittens x
        in (extend f <<< extend g) q === extend (f <<< extend g) q
    test "Comonad" do
      Assert.equal 3 (extract (kittens 3))
      quickCheck \x → (extract <<= intKittens x) === intKittens x
      quickCheck \x → 
        let
          q = intKittens x
	  f = \(Quantity i) → 2 * i
        in (extract (f <<= q)) === f q
    test "Show" do
      Assert.equal "3.5" (show (kittens 3.5))
      Assert.equal "[1,2,3]" (show (kittens [1, 2, 3]))
    test "Semigroup" do
      Assert.equal (kittens "10") (append (kittens "1") (kittens "0"))
      quickCheck \x y z → 
        let
          kx = stringKittens x
          ky = stringKittens y
          kz = stringKittens z
        in (kx <> ky) <> kz === kx <> (ky <> kz)
    test "Monoid" do
      quickCheck \x → mempty <> stringKittens x === stringKittens x
      quickCheck \x → stringKittens x <> mempty === stringKittens x
  suite "Operations" do
    test "addQ" do
      quickCheck \x y → intKittens x .+. intKittens y === kittens (x + y)
    test "subQ" do
      quickCheck \x y → intKittens x .-. intKittens y === kittens (x - y)
    test "scaleQLeft" do
      quickCheck \x y → x *. intKittens y === kittens (x * y)
    test "scaleQRight" do
      quickCheck \x y → intKittens x .* y === kittens (x * y)
  suite "Symbolled" do
    test "pretty" do
      Assert.equal "3.5 kittens" (pretty (kittens 3.5))
      Assert.equal "[1,2,3] kittens" (pretty (kittens [1, 2, 3]))      
  suite "ProductUnit" do
    test "mulQ" do
      quickCheck \x y → intKittensPerMinute x .*. intMinutes y === intKittens (x * y)
      quickCheck \x y → intMinutes x .*. intKittensPerMinute y === intKittens (x * y)
    test "divQ" do
      quickCheck \x y → kittens x ./. intMinutes y === intKittensPerMinute (x / y)
      quickCheck \x y → kittens x ./. intKittensPerMinute y === intMinutes (x / y)

