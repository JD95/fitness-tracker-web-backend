{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Queries where

import Data.Record.Anon
import qualified Db.Set as Db (Set)
import qualified Db.Workout as Db (Workout)
import qualified Model.NewSet as Model (NewSet)

type InsertSet m = "insertSet" := (Model.NewSet -> m Db.Set)

type AllWorkouts m = "allWorkouts" := m Db.Workout
