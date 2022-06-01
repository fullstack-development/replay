{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.Traced where

import Control.Monad.IO.Class
import Database.Beam.Backend.SQL
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Database.Beam.Backend.Types as Backend.Types
import qualified Database.Beam.Query as Query
import qualified Database.Beam.Postgres as Postgres

data BeamTraced be

class TracedBackendFromField be a

instance
    ( Backend.Types.BackendFromField be a
    , Aeson.ToJSON a
    , Aeson.FromJSON a
    ) =>
    TracedBackendFromField be a

instance Backend.Types.BeamBackend (BeamTraced be) where
    type BackendFromField (BeamTraced be) =
        TracedBackendFromField be

-- data Statement a where
    -- Statement ::
        -- (Aeson.FromJSON a, Aeson.ToJSON a) =>
        -- Statement a

data RecordingHandle m = RecordingHandle
    { recordReturningManyStart :: Aeson.Value -> m ()
    , recordReturningManyNext :: Aeson.Value -> m ()
    , recordReturningManyEnd :: m ()
    , recordNoReturnQuery :: Aeson.Value -> m ()
    , recordReturningOneQuery :: Aeson.Value -> Aeson.Value -> m ()
    , recordReturningListQuery :: Aeson.Value -> [Aeson.Value] -> m ()
    }

newtype BeamRecording beM a = BeamRecording
    { runBeamRecording :: beM a
    }
    deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
    (MonadBeam be beM) =>
    MonadBeam (BeamTraced be) (BeamRecording beM)
  where
    runReturningMany =
        undefined
        -- :: Beam.SQL.FromBackendRow be x =>
        -- Beam.SQL.BeamSqlBackendSyntax be -> (m (Maybe x) -> m a) -> m a
    runNoReturn =
        undefined
        -- :: Beam.SQL.BeamSqlBackendSyntax be -> m ()
    runReturningOne =
        undefined
        -- :: Beam.SQL.FromBackendRow be x =>
        -- Beam.SQL.BeamSqlBackendSyntax be -> m (Maybe x)
    runReturningList =
        undefined
        -- :: Beam.SQL.FromBackendRow be x =>
        -- Beam.SQL.BeamSqlBackendSyntax be -> m [x]

-- foo :: BeamRecording Postgres.Pg [(Integer, String, Double)]
-- foo = do
    -- undefined
    -- Query.runSelectReturningList $ Query.select $ do
        -- Query.values_
            -- [ (valGenExpr 1, valGenExpr "a", valGenExpr 2.3)
            -- ]
  -- where
    -- valGenExpr ::
        -- (HasSqlValueSyntax
            -- (Sql92ExpressionValueSyntax
                -- (Sql92SelectTableExpressionSyntax
                    -- (Sql92SelectSelectTableSyntax
                        -- (Sql92SelectSyntax
                            -- (BeamSqlBackendSyntax be)))))
            -- a) =>
        -- (BeamSqlBackend be) =>
        -- a ->
        -- Query.QGenExpr ctx be s a
    -- valGenExpr = Query.val_





data TracedSyntax syn = TracedSyntax
    { _tr :: Aeson.Value
    , _syn :: syn
    }

wrapInField :: Text.Text -> Aeson.Value -> Aeson.Value
wrapInField fieldName value = Aeson.object [fieldName Aeson..= value]

-- instance Sql92DisplaySyntax (TracedSyntax syn) where
    -- displaySyntax (TracedSyntax tr _) = show tr

-- instance
    -- ( IsSql92Syntax cmd
    -- , IsSql92SelectSyntax (TracedSyntax (Sql92SelectSyntax cmd))
    -- , IsSql92InsertSyntax (TracedSyntax (Sql92InsertSyntax cmd))
    -- , IsSql92UpdateSyntax (TracedSyntax (Sql92UpdateSyntax cmd))
    -- , IsSql92DeleteSyntax (TracedSyntax (Sql92DeleteSyntax cmd))
    -- ) =>
    -- IsSql92Syntax (TracedSyntax cmd)
  -- where
    -- type Sql92SelectSyntax (TracedSyntax cmd) =
        -- TracedSyntax (Sql92SelectSyntax cmd)
    -- type Sql92InsertSyntax (TracedSyntax cmd) =
        -- TracedSyntax (Sql92InsertSyntax cmd)
    -- type Sql92UpdateSyntax (TracedSyntax cmd) =
        -- TracedSyntax (Sql92UpdateSyntax cmd)
    -- type Sql92DeleteSyntax (TracedSyntax cmd) =
        -- TracedSyntax (Sql92DeleteSyntax cmd)

    -- selectCmd cmd =
        -- TracedSyntax (wrapInField "select" (_tr cmd)) (selectCmd (_syn cmd))
    -- insertCmd cmd =
        -- TracedSyntax (wrapInField "insert" (_tr cmd)) (insertCmd (_syn cmd))
    -- deleteCmd cmd =
        -- TracedSyntax (wrapInField "delete" (_tr cmd)) (deleteCmd (_syn cmd))
    -- updateCmd cmd =
        -- TracedSyntax (wrapInField "update" (_tr cmd)) (updateCmd (_syn cmd))

-- instance
    -- ( IsSql92SelectSyntax select
    -- , IsSql92SelectTableSyntax (TracedSyntax (Sql92SelectSelectTableSyntax select))
    -- , IsSql92OrderingSyntax (TracedSyntax (Sql92SelectOrderingSyntax select))
    -- ) =>
    -- IsSql92SelectSyntax (TracedSyntax select)
  -- where
    -- type Sql92SelectSelectTableSyntax (TracedSyntax select) =
        -- TracedSyntax (Sql92SelectSelectTableSyntax select)
    -- type Sql92SelectOrderingSyntax (TracedSyntax select) =
        -- TracedSyntax (Sql92SelectOrderingSyntax select)
    -- selectStmt
        -- table
        -- ordList
        -- mbLimit
        -- mbOffset
      -- =
        -- TracedSyntax
            -- ( Aeson.object $
                -- []
            -- )
            -- ( selectStmt (_syn table) (map _syn ordList) mbLimit mbOffset
            -- )


    -- selectStmt :: Sql92SelectSelectTableSyntax select
                -- -> [Sql92SelectOrderingSyntax select]
                -- -> Maybe Integer
                -- -> Maybe Integer
                -- -> select
