{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( app,
    Config (..),
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (groupBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Proxy (Proxy (Proxy))
import Data.Record.Anon
import Data.Record.Anon.Simple
import Data.Time.Clock (UTCTime (utctDay))
import qualified Database.SQLite.Simple as SQ
import qualified Db.Workout as Db (Workout)
import DbId (Id (Id, values))
import Muscle (Muscle (Muscle))
import Network.Wai.Handler.Warp (run)
import PrimaryMuscle (PrimaryMuscle (PrimaryMuscle))
import Queries
import qualified Queries.Sqlite as S
import Servant
  ( Capture,
    Get,
    JSON,
    Post,
    Proxy (Proxy),
    QueryParam,
    Raw,
    ReqBody,
    Server,
    serve,
    serveDirectoryWebApp,
    type (:<|>) (..),
    type (:>),
  )
import Time (Weeks (Weeks), sameDay)
import Workout (Workout (Workout))
import WorkoutSet (WorkoutSet (MkWorkoutSet, setDate, setWorkout))

type API =
  ("workouts" :> Get '[JSON] [Id Workout])
    :<|> ("muscles" :> Get '[JSON] [Id Muscle])
    :<|> ("primary-muscles" :> Get '[JSON] [PrimaryMuscle])
    :<|> ("workout" :> Capture "workout-id" Int :> "sets" :> Get '[JSON] [[Id WorkoutSet]])
    :<|> ("sets" :> QueryParam "weeks" Int :> Get '[JSON] [[Id WorkoutSet]])
    :<|> ("sets" :> ReqBody '[JSON] WorkoutSet :> Post '[JSON] (Id WorkoutSet))
    :<|> Raw

newtype Env = Env {db :: SQ.Connection}

server :: Env -> Server API
server (Env db) =
  workouts
    :<|> getMuscles
    :<|> getPrimaryMuscles
    :<|> getSetsForWorkout
    :<|> getSets
    :<|> postSets
    :<|> static
  where
    static = serveDirectoryWebApp "frontend"

    getMuscles = do
      muscles <- liftIO $ S.allMuscles db
      pure [Id i (Muscle a b c d e) | (i, a, b, c, d, e) <- muscles]

    getPrimaryMuscles = do
      primaryMuscles <- liftIO $ S.allPrimaryMuscles db
      pure [PrimaryMuscle a b | (a, b) <- primaryMuscles]

    getSets mweeks = do
      let weeks = fromMaybe 0 mweeks
      setsPerWeek <- liftIO $ S.previousSets (Time.Weeks weeks) db
      pure $ fmap (\set -> [Id i (MkWorkoutSet a b c d e) | (i, a, b, c, d, e) <- set]) setsPerWeek

    postSets ws@(MkWorkoutSet workout reps date weight intensity) = do
      liftIO $ do
        S.insertSet db (workout, reps, date, weight, intensity)
        i <- SQ.lastInsertRowId db
        pure $ Id (fromIntegral i) ws

    getSetsForWorkout workout = do
      liftIO $ do
        sets <- S.setsForWorkout db workout 10
        pure $ groupSets $ fmap (\(S.DbSet (i, a, b, c, d, e)) -> Id i (MkWorkoutSet a b c d e)) sets

    workouts = liftIO $ do
      ws <- S.allWorkouts db
      pure [Id i (Workout n) | (i, n) <- ws]

workouts' :: (Monad m, RowHas (AllWorkouts m) r) => Record r -> m Db.Workout
workouts' db = get #allWorkouts db

data Config = Config

eqOn :: Eq b => (a -> b) -> (a -> a -> Bool)
eqOn f x y = f x == f y

groupSets :: [Id WorkoutSet] -> [[Id WorkoutSet]]
groupSets = groupBy $ \x y ->
  Time.sameDay (setDate $ values x) (setDate $ values y)

app :: Config -> IO ()
app config = do
  putStrLn "Starting fitness tracker on port 8081"
  SQ.withConnection "test.db" $ \conn -> do
    SQ.setTrace conn $ Just print
    run 8081 (serve (Proxy @API) (server $ Env conn))
