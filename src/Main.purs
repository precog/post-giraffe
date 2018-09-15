module Main where

import Prelude

import Control.Coroutine (Consumer, Producer, await, emit, runProcess, ($$))
import Control.Monad.Rec.Class (Step(..), forever, tailRec)
import Control.Monad.State (get, modify)
import Control.Monad.State.Trans (evalStateT)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (Json, jsonEmptyObject, stringify, (:=), (~>))
import Data.Array.NonEmpty (NonEmptyArray, fromArray, fromNonEmpty, head, length, tail, (!!), (..), (:))
import Data.DateTime.Instant (instant, toDateTime)
import Data.Int (floor, toNumber)
import Data.JSDate (fromDateTime, toISOString)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.NonEmpty (NonEmpty(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (sum, traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Random (random)
import Math (exp, pow)
import Names (names)
import Partial.Unsafe (unsafePartial)
import UUID as UUID

data Cohort =
  Expert | Novice | Spammer

data Event
  = Registered
  | Sent
  | MarkedAsSpam

newtype UnitInterval =
  UnitInterval Number

derive newtype instance semiringUnitInterval :: Semiring UnitInterval
derive newtype instance ringUnitInterval :: Ring UnitInterval
derive newtype instance commutativeRingUnitInterval :: CommutativeRing UnitInterval
derive newtype instance euclidianRingUnitInterval :: EuclideanRing UnitInterval
derive newtype instance eqInterval :: Eq UnitInterval
derive newtype instance ordInterval :: Ord UnitInterval

newtype UnnormalisedRandom =
  UnnormalisedRandom UnitInterval

newtype NormalisedRandom =
  NormalisedRandom UnitInterval

newtype GenerationProgress =
  GenerationProgress UnitInterval

derive newtype instance semiringGenerationProgress :: Semiring GenerationProgress
derive newtype instance ringGenerationProgress :: Ring GenerationProgress

type Probability a =
  { probability :: UnitInterval, value :: a }

newtype Probabilities a =
  Probabilities (NonEmptyArray (Probability a))

type Trend =
  { apply :: Number -> Number
  , min :: Number
  , max :: Number
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

uiOne :: UnitInterval
uiOne =
  UnitInterval 1.0

uiZero :: UnitInterval
uiZero =
  UnitInterval 0.0

rand :: Effect UnnormalisedRandom
rand =
  UnnormalisedRandom <<< UnitInterval <$> random

normalRand :: Effect NormalisedRandom
normalRand =
  NormalisedRandom <<< UnitInterval <<< sum <$> (const random) `traverse` (1 .. 12)

-- Arranges a collection of probabilities on the unit line
probabilities :: forall a. NonEmptyArray (Probability a) -> Probabilities a
probabilities probabilities' =
  Probabilities
    $ (\pr -> pr { probability = pr.probability  * (uiOne / sum (_.probability <$> probabilities')) })
        <$> probabilities'

-- Picks a probability from probabilities arranged on the unit line based on a unit interval
-- Could be done with Elgot instead of TailRec but whatevs
pickProbability :: forall a. Probabilities a -> UnnormalisedRandom -> a
pickProbability (Probabilities probabilities') (UnnormalisedRandom r) =
  tailRec go { progress: uiZero, stillToGo: probabilities' }
  where
  go
    :: { progress :: UnitInterval, stillToGo :: NonEmptyArray (Probability a) }
    -> Step { progress :: UnitInterval, stillToGo :: NonEmptyArray (Probability a) } a
  go { progress, stillToGo } =
    case fromArray $ tail stillToGo  of
      Nothing ->
        Done (head stillToGo).value
      Just tail' ->
        if (head stillToGo).probability + progress >= r
          then
            Done (head stillToGo).value
          else
            Loop
              { progress: (head stillToGo).probability + progress
              , stillToGo: tail'
              }

cohorts :: Probabilities Cohort
cohorts =
  probabilities
    $ fromNonEmpty
    $ NonEmpty
        { probability: UnitInterval 0.2, value: Expert }
        [ { probability: UnitInterval 0.1, value: Spammer }
        , { probability: UnitInterval 0.8, value: Novice }
        ]

type Registration =
  { id :: String, cohort :: Cohort, registeredAt :: GenerationProgress }

type SentEmail =
  { id :: String, rId :: String, markedAsSpamProbability :: UnitInterval }

initialRegistration :: NonEmptyArray Registration
initialRegistration =
  fromNonEmpty
    $ NonEmpty
        { id: "0870b45c-3922-42fd-92ca-662b3636f78d"
        , cohort: Expert
        , registeredAt: GenerationProgress $ UnitInterval 0.0
        }
        []

initialSentEmail :: NonEmptyArray SentEmail
initialSentEmail =
  fromNonEmpty
    $ NonEmpty
        { id: "6876ccde-c17a-4f0e-a6c3-6b90a84a3570"
        , rId: "0870b45c-3922-42fd-92ca-662b3636f78d"
        , markedAsSpamProbability: UnitInterval 0.0
        }
        []

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

fromUI :: UnitInterval -> Number
fromUI (UnitInterval ui) = ui

rescale :: MaxRows -> RowNumber -> GenerationProgress
rescale (MaxRows maxRows) (RowNumber rowNumber) =
  GenerationProgress
    $ UnitInterval
    $ toNumber rowNumber / toNumber maxRows

intScale :: Int -> Int -> UnitInterval -> Int
intScale min max (UnitInterval ui) =
  floor (ui * (toNumber max - toNumber min)) + min

-- Maybe it would be useful to make calculating trends always return unit
-- intervals then allow this to be converted into other stuff like numbers
-- or date time strings afterwards if desired.
applyTrend :: UnitInterval -> Trend -> Number
applyTrend (UnitInterval ui) trend =
  (trend.apply ui) * (trend.max - trend.min) + trend.min

calculate :: DeviatingTrend -> GenerationProgress -> NormalisedRandom -> Number
calculate t (GenerationProgress p) (NormalisedRandom (UnitInterval r)) =
  applyTrend p (trendToTrend t) + r * (applyTrend p (deviationToTrend t))

inBounds :: MaxRows -> RowNumber -> Boolean
inBounds (MaxRows maxRows) (RowNumber rowNumber) =
  maxRows > rowNumber

element :: forall a. NonEmptyArray a -> UnnormalisedRandom -> a
element xs (UnnormalisedRandom r) =
  unsafePartial $ fromJust $ (xs !! _) $ intScale 0 (length xs - 1) r

producer :: MaxRows -> Producer Json Aff Unit
producer maxRows =
  flip evalStateT
    { registrations: initialRegistration, sent: initialSentEmail }
    $ go (RowNumber 0)
  where
  go rowNumber =
    when (inBounds maxRows rowNumber) do
      let progress = rescale maxRows rowNumber

      normalRand' <- lift $ lift $ liftEffect $ normalRand
      rand' <- lift $ lift $ liftEffect $ rand
      previous <- get
      datetime' <- maybe
                     (pure "")
                     (lift
                        <<< lift
                        <<< liftEffect
                        <<< toISOString
                        <<< fromDateTime
                        <<< toDateTime)
                        $ instant
                        $ Milliseconds
                        $ calculate datetime progress normalRand'
      uuid1 <- lift $ UUID.toString <$> UUID.make
      uuid2 <- lift $ UUID.toString <$> UUID.make

      let randomName = element names rand'
      let randomPreviousRegistration = element previous.registrations rand'
      let randomPreviousSent = element previous.sent rand'
      let randomCohort = pickProbability cohorts rand'
      let event = pickProbability
                   (probabilities
                      $ fromNonEmpty
                      $ NonEmpty
                          { probability:
                              randomPreviousSent.markedAsSpamProbability
                          , value:
                              MarkedAsSpam
                          }
                          [ { probability:
                                UnitInterval
                                  $ calculate
                                      (sentProbability randomPreviousRegistration.cohort)
                                      (progress - randomPreviousRegistration.registeredAt)
                                      normalRand'
                            , value:
                                Sent
                            }
                          , { probability:
                                UnitInterval 1.0
                            , value:
                                Registered
                            }
                          ])
                   rand'

      case event of
        MarkedAsSpam -> do
          lift $ emit
            $ uuid1
                := ("dateTime" := datetime'
                    ~> "MAS" :=
                         ("rId" := randomPreviousSent.rId
                            ~> "sId" := randomPreviousSent.id
                            ~> jsonEmptyObject)
                    ~> jsonEmptyObject)
                ~> jsonEmptyObject
        Sent -> do
          _ <- modify
                 (\s' ->
                    s' { sent =
                           { id:
                               uuid1
                           , rId:
                               uuid2
                           , markedAsSpamProbability:
                               UnitInterval
                                 $ calculate
                                     (markedAsSpamProbability randomCohort)
                                     progress
                                     normalRand'
                           }
                             : s'.sent
                       })
          lift $ emit
            $ uuid1 :=
                ("dateTime" := datetime'
                    ~> "S" :=
                      ("rId" := uuid2
                         ~> "to" := (randomName <> "@example.com")
                         ~> "cc" := "example@example.com"
                         ~> "bcc" := "example@example.com"
                         ~> "subject" := "Example"
                         ~> "body" := "Example"
                         ~> jsonEmptyObject)
                    ~> jsonEmptyObject)
                ~> jsonEmptyObject
        Registered -> do
          lift $ emit
            $ uuid1
                := ("dateTime" := datetime'
                      ~> ("R"
                            := ("email" := (randomName <> "@example.com")
                                  ~> "name" := randomName
                                  ~> jsonEmptyObject)
                            ~> jsonEmptyObject))
                ~> jsonEmptyObject
      go $ (\(RowNumber i) -> RowNumber $ i + 1) rowNumber

consumer :: Consumer Json Aff Unit
consumer =
  forever
    $ (await >>= stringify >>> log >>> liftEffect >>> lift) $> Nothing

-- Time advances throughout the generation but the rate at which it
-- advances slows down over the generation making more events happen at later times.
datetime :: DeviatingTrend
datetime =
  { apply: \x -> 1.000471 - 1.000471* exp (-7.661191 * x)
  , min: 1518610203000.0
  , max: 1528061540000.0
  , deviation: \x -> 0.08784608 + 0.9121539 * exp (-4.318157 * x)
  , dMin: 10.0
  , dMax: 60000.0
  }

-- It might be stronger to always have the trends go from 0 to 1 then scale and offset
-- them afterwards.
markedAsSpamProbability :: Cohort -> DeviatingTrend
markedAsSpamProbability =
  case _ of
    Expert ->
      { apply: const 0.1
      , min: 0.3
      , max: 0.5
      , deviation: const 0.01
      , dMin: 0.001
      , dMax: 0.1
      }
    Novice ->
      { apply: \x -> 0.06327665 + (0.4037157 - 0.06327665)/(1.0 + (x/0.5141939) `pow` 3.689102)
      , min: 0.002
      , max: 0.2
      , deviation: const 0.00001
      , dMin: 0.0001
      , dMax: 0.01
      }
    Spammer ->
      { apply: const 0.4
      , min: 0.3
      , max: 0.5
      , deviation: const 0.01
      , dMin: 0.001
      , dMax: 0.1
      }

-- This models the send limits imposed by Post Giraffe. In imagination land every user has an
-- initial send limit which increases relative to the amount of their limit they use up and
-- the proportion of mail they send which is marked as spam. In reality each cohort has a
-- trend which controls how likely it is that they send an email in a given point in the
-- generation offset by the point in the generation which they registered.
sentProbability :: Cohort -> DeviatingTrend
sentProbability =
  case _ of
    Expert ->
      { apply: \x -> 1.0 + (0.1 - 1.0)/(1.0 + (x/0.2017754) `pow` 25.24803)
      , min: 0.05
      , max: 1.0
      , deviation: const 0.01
      , dMin: 0.001
      , dMax: 0.1
      }
    Novice ->
      { apply: \x -> 490340.3 + (0.1040133 - 490340.3)/(1.0 + (x/39.99118) `pow` 3.582534)
      , min: 0.05
      , max: 1.0
      , deviation: const 0.01
      , dMin: 0.001
      , dMax: 0.01
      }
    Spammer ->
      { apply: const 0.1
      , min: 0.05
      , max: 0.15
      , deviation: const 0.01
      , dMin: 0.001
      , dMax: 0.1
      }

main :: Effect Unit
main =
  void $ launchAff $ runProcess (producer (MaxRows 10000) $$ consumer)
