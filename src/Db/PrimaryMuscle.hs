{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Db.PrimaryMuscle where

import Data.Text (Text)
import Data.UUID
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP
import Squeal.PostgreSQL

type Columns =
  '[ "id" ::: 'Def :=> 'NotNull 'PGuuid,
     "workout" ::: 'NoDef :=> 'NotNull 'PGuuid,
     "muscle" ::: 'NoDef :=> 'NotNull 'PGuuid
   ]

type Constraints =
  '[ "pk_primary_muscle" ::: 'PrimaryKey '["id"],
     "fk_workout_id" ::: 'ForeignKey '["workout"] "fitness_tracker" "workout" '["id"],
     "fk_muscle_id" ::: 'ForeignKey '["muscle"] "fitness_tracker" "muscle" '["id"]
   ]

type Schema = "primary_muscle" ::: 'Table (Constraints :=> Columns)

data PrimaryMuscle = PrimaryMuscle {id :: UUID, workout :: UUID, muscle :: UUID}
  deriving stock (Show, Eq, GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
