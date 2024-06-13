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
import qualified Db.Schema as Db
import qualified Db.Set as Db (Set)
import qualified Db.SetIntensity as Db (PGsetintensity, SetIntensity (..))
import qualified Db.Workout as Db (Workout)
import GHC.Int
import qualified Model.NewSet as Model (NewSet (..))
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
  Model.workout
    .* (encInt . Model.reps)
    .* Model.time
    .* Model.weight
    *. (Db.SetIntensity . Model.intensity)
  where
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

allWorkouts :: Statement Db.Schema () Db.Workout
allWorkouts =
  query $
    select_
      ( #workout ! #id `as` #id
          :* #workout ! #name `as` #name
      )
      (from $ table $ #fitness_tracker ! #workout)
