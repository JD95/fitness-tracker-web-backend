{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Db.Schema where

import qualified Db.Muscle as Muscle
import qualified Db.PrimaryMuscle as PrimaryMuscle
import qualified Db.Set as Set
import qualified Db.SetIntensity as SetIntensity
import qualified Db.Workout as Workout
import Squeal.PostgreSQL

type Schema =
  '[ "fitness_tracker"
       ::: '[ Workout.Schema,
              Set.Schema,
              SetIntensity.Schema,
              Muscle.Schema,
              PrimaryMuscle.Schema
            ]
   ]
