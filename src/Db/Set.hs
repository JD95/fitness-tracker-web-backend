{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Db.Set where

import Data.Text (Text)
import Data.Time.Clock
import Data.UUID
import Db.SetIntensity
import qualified GHC.Generics as GHC
import GHC.Int
import qualified Generics.SOP as SOP
import Squeal.PostgreSQL

type Columns =
  '[ "id" ::: 'Def :=> 'NotNull 'PGuuid,
     "workout" ::: 'NoDef :=> 'NotNull 'PGuuid,
     "reps" ::: 'NoDef :=> 'NotNull 'PGint4,
     "time" ::: 'NoDef :=> 'NotNull 'PGtimestamptz,
     "weight" ::: 'NoDef :=> 'NotNull 'PGfloat4,
     "intensity" ::: 'NoDef :=> 'NotNull PGsetintensity
   ]

type Constraints =
  '[ "pk_set" ::: 'PrimaryKey '["id"],
     "fk_workout_id" ::: 'ForeignKey '["workout"] "fitness_tracker" "workout" '["id"]
   ]

type Schema = "set" ::: 'Table (Constraints :=> Columns)

data Set = Set
  { id :: UUID,
    workout :: UUID,
    reps :: Int32,
    time :: UTCTime,
    weight :: Float,
    intensity :: SetIntensity
  }
  deriving stock (Show, GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
