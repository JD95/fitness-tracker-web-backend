{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Queries where

import Data.Kind
import Data.Record.Anon
import Data.UUID
import qualified Db.Set as Db (Set)
import qualified Db.Workout as Db (Workout)
import GHC.TypeLits
import qualified Model.NewSet as Model (NewSet)

type family RowHas x r :: Constraint where
  RowHas (x := a) r = RowHasField x r a

type InsertPrimaryMuscle m = "insertPrimaryMuscle" := (UUID -> UUID -> m Db.Set)

type InsertSet m = "insertSet" := (Model.NewSet -> m Db.Set)

type AllSets m = "allSets" := m [Db.Set]

type AllWorkouts m = "allWorkouts" := m Db.Workout
