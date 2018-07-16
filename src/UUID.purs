module UUID
  ( UUIDv4
  , make
  , toString
  , fromString
  ) where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random as Random
import Control.MonadZero (guard)
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe, fromJust)
import Data.String as String
import Data.Traversable (sequence, traverse)
import Partial.Unsafe (unsafePartial)

data UUIDv4 = UUIDv4 String (Array Int)

make ∷ ∀ m. MonadEffect m ⇒ m UUIDv4
make = liftEffect $ make' Random.randomInt

make' ∷ ∀ f. Applicative f ⇒ (Int → Int → f Int) → f UUIDv4
make' rand = unsafeToUUIDv4 <$> sequence uuid
  where
    uuid = x8 <> x4 <> [ pure 4 ] <> x3 <> y <> x3 <> x12
    y = [ rand 8 11 ]
    x = [ rand 0 15 ]
    x3 = x <> x <> x
    x4 = x3 <> x
    x8 = x4 <> x4
    x12 = x8 <> x4

toString ∷ UUIDv4 → String
toString (UUIDv4 s _) = s

toString' ∷ Array Int → String
toString' is =
  String.joinWith "-"
    [ print 0 8
    , print 8 12
    , print 12 16
    , print 16 20
    , print 20 32
    ]
  where
    print i j =
      Array.slice i j is
        # map (Int.toStringAs hexRadix)
        # String.joinWith ""

fromString ∷ String → Maybe UUIDv4
fromString str = do
  let
    ds = String.split (String.Pattern "") =<< String.split (String.Pattern "-") str
  is ← traverse (Int.fromStringAs hexRadix) ds
  rd ← Array.index is 12
  yd ← Array.index is 16
  guard $ Array.length is == 32
  guard $ rd == 4
  guard $ yd >= 8 && yd < 12
  pure (unsafeToUUIDv4 is)

unsafeToUUIDv4 ∷ Array Int → UUIDv4
unsafeToUUIDv4 is = UUIDv4 (toString' is) is

hexRadix ∷ Int.Radix
hexRadix = unsafePartial (fromJust (Int.radix 16))

instance eqUUIDv4 ∷ Eq UUIDv4 where
  eq (UUIDv4 s1 _) (UUIDv4 s2 _) = s1 == s2

instance ordUUIDv4 ∷ Ord UUIDv4 where
  compare (UUIDv4 s1 _) (UUIDv4 s2 _) = compare s1 s2

instance showUUIDv4 ∷ Show UUIDv4 where
  show (UUIDv4 s1 _) = "UUIDv4 " <> show s1
