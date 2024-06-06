{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Queries.Squeal where

import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Db.Schema as Db
import qualified Db.Workout as Db (Workout)
import Squeal.PostgreSQL

allWorkouts :: (MonadIO m, MonadPQ Db.Schema m) => m [Db.Workout]
allWorkouts = getRows =<< execute stmt
  where
    stmt :: Statement Db.Schema () Db.Workout
    stmt =
      query $
        select_
          ( #workout ! #id `as` #id
              :* #workout ! #name `as` #name
          )
          (from (table #workout))
