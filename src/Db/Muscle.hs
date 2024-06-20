{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Db.Muscle where

import Data.Text (Text)
import Data.UUID
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP
import Squeal.PostgreSQL

type Columns =
  '[ "id" ::: 'Def :=> 'NotNull 'PGuuid,
     "name" ::: 'NoDef :=> 'NotNull 'PGtext
   ]

type Constraints =
  '["pk_muscle" ::: 'PrimaryKey '["id"]]

type Schema = "muscle" ::: 'Table (Constraints :=> Columns)

data Muscle = Muscle {id :: UUID, name :: Text}
  deriving stock (Show, Eq, GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
