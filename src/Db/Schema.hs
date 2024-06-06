{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Db.Schema where

import qualified Db.Workout as Workout
import Squeal.PostgreSQL

type Schema = '["fitness_tracker" ::: '[Workout.Schema]]
