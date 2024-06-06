{-# LANGUAGE DataKinds #-}

module Db.Schema where

import qualified Db.Workout as Workout
import Squeal.PostgreSQL

type Schema = Public '[Workout.Schema]
