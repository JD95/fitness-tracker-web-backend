{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Queries.Squeal (execute, insertSet, allWorkouts) where

import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Time.Clock
import Data.UUID
import qualified Db.Muscle as Db (Muscle)
import qualified Db.Schema as Db
import qualified Db.Set as Db (Set)
import qualified Db.SetIntensity as Db (PGsetintensity, SetIntensity (..))
import qualified Db.Workout as Db (Workout)
import GHC.Int
import qualified Model.NewMuscle as Model (NewMuscle (..))
import qualified Model.NewMuscle as NewMuscle
import qualified Model.NewPrimaryMuscle as Model (NewPrimaryMuscle (..))
import qualified Model.NewPrimaryMuscle as NewPrimaryMuscle
import qualified Model.NewSet as Model (NewSet (..))
import qualified Model.NewSet as NewSet
import qualified Model.NewWorkout as Model (NewWorkout (..))
import qualified Model.NewWorkout as NewWorkout
import qualified Model.SetIntensity as Model (SetIntensity)
import Squeal.PostgreSQL hiding (execute)
import qualified Squeal.PostgreSQL as Squeal

execute :: (MonadIO m, MonadPQ Db.Schema m) => Statement Db.Schema () a -> m [a]
execute stmt = getRows =<< Squeal.execute stmt

class Conversion model db where
  toDb :: model -> db
  fromDb :: db -> model

instance Conversion Model.SetIntensity Db.SetIntensity where
  toDb = Db.SetIntensity
  fromDb (Db.SetIntensity x) = x

encodeNewSet ::
  EncodeParams
    Db.Schema
    '[ 'NotNull 'PGuuid,
       'NotNull 'PGint4,
       'NotNull 'PGtimestamptz,
       'NotNull 'PGfloat4,
       'NotNull Db.PGsetintensity
     ]
    Model.NewSet
encodeNewSet =
  NewSet.workout
    .* (encInt . Model.reps)
    .* Model.time
    .* Model.weight
    *. (Db.SetIntensity . Model.intensity)

encInt :: Int -> Int32
encInt = fromInteger . toInteger

insertSet :: Statement Db.Schema Model.NewSet ()
insertSet = Manipulation encodeNewSet genericRow stmt
  where
    stmt =
      insertInto_
        (#fitness_tracker ! #set)
        (Values_ values)

    values =
      Default `as` #id
        :* Set (param @1) `as` #workout
        :* Set (param @2) `as` #reps
        :* Set (param @3) `as` #time
        :* Set (param @4) `as` #weight
        :* Set (param @5) `as` #intensity

insertPrimaryMuscle :: Statement Db.Schema Model.NewPrimaryMuscle ()
insertPrimaryMuscle = Manipulation enc genericRow stmt
  where
    enc = NewPrimaryMuscle.workout *. NewPrimaryMuscle.muscle
    stmt =
      insertInto_
        (#fitness_tracker ! #primary_muscle)
        (Values_ values)

    values =
      Default `as` #id
        :* Set (param @1) `as` #workout
        :* Set (param @2) `as` #muscle

allSets :: Statement Db.Schema () Db.Set
allSets =
  query $
    select_
      ( #set ! #id `as` #id
          :* #set ! #workout `as` #workout
          :* #set ! #reps `as` #reps
          :* #set ! #time `as` #time
          :* #set ! #weight `as` #weight
          :* #set ! #intensity `as` #intensity
      )
      (from $ table $ #fitness_tracker ! #set)

previousSetsForWorkout :: Statement Db.Schema (UTCTime, UTCTime, UUID) Db.Set
previousSetsForWorkout =
  query $
    select_
      ( #set ! #id `as` #id
          :* #set ! #workout `as` #workout
          :* #set ! #reps `as` #reps
          :* #set ! #time `as` #time
          :* #set ! #weight `as` #weight
          :* #set ! #intensity `as` #intensity
      )
      ( from (table $ #fitness_tracker ! #set)
          & where_
            ( (param @1 .< #time)
                .&& (#time .< param @2)
                .&& (#workout .== param @3)
            )
      )

previousSets :: Statement Db.Schema (UTCTime, UTCTime) Db.Set
previousSets =
  query $
    select_
      ( #set ! #id `as` #id
          :* #set ! #workout `as` #workout
          :* #set ! #reps `as` #reps
          :* #set ! #time `as` #time
          :* #set ! #weight `as` #weight
          :* #set ! #intensity `as` #intensity
      )
      ( from (table $ #fitness_tracker ! #set)
          & where_
            ( (param @1 .< #time)
                .&& (#time .< param @2)
            )
      )

insertWorkout :: Statement Db.Schema Model.NewWorkout ()
insertWorkout =
  Manipulation (NewWorkout.name .* nilParams) genericRow $
    insertInto_
      (#fitness_tracker ! #workout)
      (Values_ values)
  where
    values =
      Default `as` #id
        :* Set (param @1) `as` #name

insertMuscle :: Statement Db.Schema Model.NewMuscle ()
insertMuscle =
  Manipulation (NewMuscle.name .* nilParams) genericRow $
    insertInto_
      (#fitness_tracker ! #muscle)
      (Values_ values)
  where
    values =
      Default `as` #id
        :* Set (param @1) `as` #name

allWorkouts :: Statement Db.Schema () Db.Workout
allWorkouts =
  query $
    select_
      ( #workout ! #id `as` #id
          :* #workout ! #name `as` #name
      )
      (from $ table $ #fitness_tracker ! #workout)

allMuscles :: Statement Db.Schema () Db.Muscle
allMuscles =
  query $
    select_
      ( #muscle ! #id `as` #id
          :* #muscle ! #name `as` #name
      )
      (from $ table $ #fitness_tracker ! #muscle)
