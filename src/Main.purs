module Main where

import Prelude

import Control.Coroutine (Consumer, Producer, await, emit, runProcess, ($$))
import Control.Monad.Rec.Class (Step(..), forever, tailRec, tailRecM)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (Json, jsonEmptyObject, stringify, (:=), (~>))
import Data.Array (length, (!!), (..), (:))
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (sum, traverse)
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Console (log)
import Effect.Random (random, randomInt)
import Math (abs, exp, pow)
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

newtype RowNumber = RowNumber Number

newtype MaxRows = MaxRows Number

newtype UnitInterval = UnitInterval Number

fromUI :: UnitInterval -> Number
fromUI (UnitInterval ui) = ui

normalRand :: Effect UnitInterval
normalRand =
  UnitInterval <<< sum <$> (const random) `traverse` (1 .. 12)

rescale :: MaxRows -> RowNumber -> UnitInterval
rescale (MaxRows maxRows) (RowNumber rowNumber) =
  UnitInterval $ rowNumber / maxRows

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
  "R" :=
    ("email" := "example@example.com"
       ~> "name" := "Example Example"
       ~> jsonEmptyObject)

other :: Array _
other =
  [ other'
  , "UPD" :=
      ("rId" := "examplea-8ea0-4151-99fc-6b25decba280"
         ~> "token" := "ExamplevBiI2HlWgH4olfQ2"
         ~> jsonEmptyObject)
  , "DSLU" :=
      ("rId" := "examplea-8ea0-4151-99fc-6b25decba280"
         ~> "prev" := 0
         ~> "today" := 200
         ~> jsonEmptyObject)
  , "AFA" :=
      ("rId" := "examplea-8ea0-4151-99fc-6b25decba280"
         ~> "address" := "example@example.com"
         ~> jsonEmptyObject)
  , "VFA" :=
      ("rId" := "examplea-8ea0-4151-99fc-6b25decba280"
         ~> "afaId" := "example2-17c5-4abf-90b7-81ff1831d079"
         ~> jsonEmptyObject)
  ]

p :: MaxRows -> Effect Unit
p maxRows =
  tailRecM go 0.0
  where
  go i =
    if inBounds maxRows (RowNumber i)
      then do
        datetime' <- calculate' i datetime
        markedAsSpamProbability' <- calculate' i markedAsSpamProbability
        random' <- random
        id <- UUID.toString <$> UUID.make
        rId <- UUID.toString <$> UUID.make
        sId <- UUID.toString <$> UUID.make
        email' <- email
        log $ stringify
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
        pure $ Loop $ i + 1.0
      else pure $ Done unit

  calculate' i =
    calculate (generationProgress i)

  generationProgress i =
    rescale maxRows $ RowNumber i

datetime :: DeviatingTrend
datetime =
  { apply: \x -> 1.000471 - 1.000471* exp (-7.661191 * x)
  , min: 1518610203000.0
  , max: 1528061540000.0
  , deviation: \x -> 0.08784608 + 0.9121539 * exp (-4.318157 * x)
  , dMin: 10.0
  , dMax: 60000.0
  }

markedAsSpamProbability :: DeviatingTrend
markedAsSpamProbability =
  { apply: \x -> 1.004291 + (1.582493e-17 - 1.004291)/(1.0 + (x/0.3177867) `pow` 4.755252)
  , min: 0.1
  , max: 0.2
  , deviation: const 0.00001
  , dMin: 0.0001
  , dMax: 0.01
  }

main :: Effect Unit
main =
  void $ p (MaxRows 28783026905.0)
