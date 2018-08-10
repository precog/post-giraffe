module Main where

import Prelude

import Control.Coroutine (Consumer, Producer, await, emit, runProcess, ($$))
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (Json, jsonEmptyObject, stringify, (:=), (~>))
import Data.Array (length, (!!), (..), (:))
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (sum, traverse)
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Random (random, randomInt)
import Math (abs, pow)
import Names (names)
import UUID as UUID

type Trend =
  { apply :: Number -> Number
  , min :: Number
  , max :: Number
  }

instance eqInterval :: Eq UnitInterval where
  eq (UnitInterval x) (UnitInterval y) = x `eq` y

instance ordInterval :: Ord UnitInterval where
  compare (UnitInterval x) (UnitInterval y) = x `compare` y

trendToTrend :: DeviatingTrend -> Trend
trendToTrend { apply, min, max } =
  { apply, min, max }

deviationToTrend :: DeviatingTrend -> Trend
deviationToTrend { deviation, dMin, dMax } =
  { apply: deviation, min: dMin, max: dMax }

toDeviatingTrend :: Trend -> DeviatingTrend
toDeviatingTrend { apply, min, max } =
  { apply
  , min
  , max
  , deviation: const 0.0
  , dMin: 0.0
  , dMax: 0.0
  }

type DeviatingTrend =
  { apply :: Number -> Number
  , min :: Number
  , max :: Number
  , deviation :: Number -> Number
  , dMin :: Number
  , dMax :: Number
  }

newtype RowNumber = RowNumber Int

newtype MaxRows = MaxRows Int

newtype UnitInterval = UnitInterval Number

fromUI :: UnitInterval -> Number
fromUI (UnitInterval ui) = ui

normalRand :: Effect UnitInterval
normalRand =
  UnitInterval <<< sum <$> (const random) `traverse` (1 .. 12)

rescale :: MaxRows -> RowNumber -> UnitInterval
rescale (MaxRows maxRows) (RowNumber rowNumber) =
  UnitInterval $ toNumber rowNumber / toNumber maxRows

intScale :: Int -> Int -> UnitInterval -> Int
intScale min max (UnitInterval ui) =
  floor (ui * (toNumber max - toNumber min)) + min

applyTrend :: UnitInterval -> Trend -> Number
applyTrend (UnitInterval ui) trend =
  (trend.apply ui) * (trend.max - trend.min) + trend.min

inBounds :: MaxRows -> RowNumber -> Boolean
inBounds (MaxRows maxRows) (RowNumber rowNumber) =
  maxRows > rowNumber

calculate :: UnitInterval -> DeviatingTrend -> Effect Number
calculate generationProgress trend =
  (\(UnitInterval r) -> applyTrend generationProgress (trendToTrend trend) + r * (applyTrend generationProgress (deviationToTrend trend)))
    <$> normalRand

email :: Effect String
email =
  (_ <> "@example.com") <<< fromMaybe "" <<< (names !! _) <$> randomInt 0 (length names - 1)

other' :: _
other' =
  "R" := 0

other :: Array _
other =
  [ other'
  , "UPD" := 0
  , "DSLU" := 0
  , "AFA" := 0
  , "VFA" := 0
  ]

p :: MaxRows -> Producer Json Aff Unit
p maxRows =
  go 0
  where
  go i =
    if inBounds maxRows (RowNumber i)
      then do
        datetime' <- calculate' i datetime
        markedAsSpamProbability' <- calculate' i markedAsSpamProbability
        random' <- lift $ liftEffect $ random
        id <- UUID.toString <$> UUID.make
        rId <- UUID.toString <$> UUID.make
        sId <- UUID.toString <$> UUID.make
        email' <- lift $ liftEffect email
        emit
          $ if random' < (markedAsSpamProbability' / 2.0)
              then
                id :=
                 ("dateTime" := datetime'
                     ~> "MAS" :=
                          ("rId" := rId
                             ~> "sId" := sId
                             ~> jsonEmptyObject)
                     ~> jsonEmptyObject)
                 ~> jsonEmptyObject
              else if random' >= (markedAsSpamProbability' / 2.0) && random' < 0.5
                then
                  id :=
                   ("dateTime" := datetime'
                       ~> "S" :=
                        ("rId" := rId
                           ~> (if (generationProgress i) > (UnitInterval 0.15) then "to" := email' else "to" := [ email' ])
                           ~> "cc" := ""
                           ~> "bcc" := ""
                           ~> "subject" := ""
                           ~> "body" := ""
                           ~> jsonEmptyObject)
                       ~> jsonEmptyObject)
                   ~> jsonEmptyObject
                else
                  id :=
                    ("dateTime" := datetime' ~> (fromMaybe other' $ other !! (intScale 0 (length other - 1) $ UnitInterval ((random' - 0.5) * 2.0))) ~> jsonEmptyObject)
                    ~> jsonEmptyObject
        go $ i + 1
      else pure unit

  calculate' i =
    lift <<< liftEffect <<< calculate (generationProgress i)

  generationProgress i =
    rescale maxRows $ RowNumber i

c :: Consumer Json Aff Unit
c =
  forever
    $ (await >>= stringify >>> log >>> liftEffect >>> lift) $> Nothing

datetime :: DeviatingTrend
datetime =
  { apply: y
  , min: 1518610203000.0
  , max: 1528061540000.0
  , deviation: d
  , dMin: 10.0
  , dMax: 60000.0
  }
  where
  d x = -0.004290714 + (1.0 + 0.004290714) / (1.0 + (x / 0.3177867) `pow` 4.755252)
  y x = 1.004291 + (1.582493e-17 - 1.004291)/(1.0 + (x/0.3177867) `pow` 4.755252)

markedAsSpamProbability :: DeviatingTrend
markedAsSpamProbability =
  datetime
    { min = 0.1
    , max = 0.2
    , dMin = 0.0001
    , dMax = 0.01
    }

main :: Effect Unit
main =
  void $ launchAff $ runProcess (p (MaxRows 10000) $$ c)
