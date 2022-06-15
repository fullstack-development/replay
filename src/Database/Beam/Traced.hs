{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.Traced where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Proxy
import Database.Beam.Backend.SQL
import Database.Beam.Backend.Types
import Database.Beam.Query
import Database.Beam.Query.Internal
import Database.Beam.Query.SQL92
import Database.Beam.Traced.Syntax
import qualified Control.Monad.Free.Church as Free.Church
import qualified Control.Record as Record
import qualified Control.Reducible as Reducible
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Database.Beam.Postgres as Postgres
import qualified Database.PostgreSQL.Simple as PS
import qualified Data.Yaml.Pretty as Yaml.Pretty
import qualified Data.Yaml.TH as Yaml.TH
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Control.Concurrent.STM

showYaml :: (Aeson.ToJSON a) => a -> String
showYaml =
    BS.Char8.unpack . Yaml.Pretty.encodePretty yamlConfig
  where
    yamlConfig =
        Yaml.Pretty.setConfCompare compare $
        Yaml.Pretty.setConfDropNull True $
        Yaml.Pretty.defConfig

data BeamTraced be

instance (BeamSqlBackend be) => BeamSqlBackend (BeamTraced be)

type instance BeamSqlBackendSyntax (BeamTraced be) =
    ParallelSyntax
        SyntaxAST
        (BeamSqlBackendSyntax be)

instance (BeamBackend be) => BeamBackend (BeamTraced be) where
    type BackendFromField (BeamTraced be) =
        BackendFromField be

newtype BTWrapper a = BTWrapper
    { unwrapBTWrapper :: a
    }

instance {-# OVERLAPPING #-} (HasQBuilder be) => HasQBuilder (BeamTraced be) where
    buildSqlQuery tablePrefix q =
        ParallelSyntax
            (getAst $ buildSql92Query' @(BeamTraced be) True tablePrefix q)
            (buildSqlQuery @be tablePrefix (fmap BTWrapper $ unliftQTraced q))

setSyn ::
    TracedSyntax syn ->
    syn ->
    TracedSyntax syn
setSyn (ParallelSyntax ast _) syn =
    ParallelSyntax ast syn

unsafeFromSyn ::
    syn -> TracedSyntax syn
unsafeFromSyn syn =
    ParallelSyntax (error "no AST") syn

instance
    ( ProjectibleWithPredicate
        contextPredicate
        (BeamTraced be)
        (WithExprContext (BeamSqlBackendExpressionSyntax' (BeamTraced be)))
        a
    ) =>
    ProjectibleWithPredicate
        contextPredicate
        be
        (WithExprContext (BeamSqlBackendExpressionSyntax' be))
        (BTWrapper a)
  where
    project' contextPredicateProxy _ fn (BTWrapper a) =
        fmap BTWrapper $
            project'
                contextPredicateProxy
                (Proxy
                    @( BeamTraced be
                     , WithExprContext
                        (BeamSqlBackendExpressionSyntax' (BeamTraced be))
                     )
                )
                (\contextProxy _ mkTracedSyn ->
                    fmap
                        (setSynP mkTracedSyn)
                        (fn contextProxy (Proxy @be) (getSynP mkTracedSyn))
                )
                a
      where
        getSynP mkTracedSyn p =
            BeamSqlBackendExpressionSyntax' $
            getSyn $
            fromBeamSqlBackendExpressionSyntax $
            mkTracedSyn p
        setSynP mkTracedSyn mkSyn p =
            case fromBeamSqlBackendExpressionSyntax $ mkTracedSyn p of
                ParallelSyntax ast _ ->
                    BeamSqlBackendExpressionSyntax' $
                        ParallelSyntax
                            ast
                            (fromBeamSqlBackendExpressionSyntax $ mkSyn p)
    projectSkeleton' contextPredicateProxy _ fn =
        fmap BTWrapper $
            projectSkeleton'
                contextPredicateProxy
                (Proxy
                    @( BeamTraced be
                     , WithExprContext
                        (BeamSqlBackendExpressionSyntax' (BeamTraced be))
                     )
                )
                (\contextProxy _ ->
                    fmap
                        unsafeFromSynP
                        (fn contextProxy (Proxy @be))
                )
      where
        unsafeFromSynP mkSyn p =
            BeamSqlBackendExpressionSyntax' $
            unsafeFromSyn $
            fromBeamSqlBackendExpressionSyntax $
            mkSyn p

instance
    ( ProjectibleWithPredicate
        contextPredicate
        (BeamTraced be)
        (WithExprContext (BeamSqlBackendWindowFrameSyntax' (BeamTraced be)))
        a
    ) =>
    ProjectibleWithPredicate
        contextPredicate
        be
        (WithExprContext (BeamSqlBackendWindowFrameSyntax' be))
        (BTWrapper a)
  where
    project' contextPredicateProxy _ fn (BTWrapper a) =
        fmap BTWrapper $
            project'
                contextPredicateProxy
                (Proxy
                    @( BeamTraced be
                     , WithExprContext
                        (BeamSqlBackendWindowFrameSyntax' (BeamTraced be))
                     )
                )
                (\contextProxy _ mkTracedSyn ->
                    fmap
                        (setSynP mkTracedSyn)
                        (fn contextProxy (Proxy @be) (getSynP mkTracedSyn))
                )
                a
      where
        getSynP mkTracedSyn p =
            BeamSqlBackendWindowFrameSyntax' $
            getSyn $
            fromBeamSqlBackendWindowFrameSyntax $
            mkTracedSyn p
        setSynP mkTracedSyn mkSyn p =
            case fromBeamSqlBackendWindowFrameSyntax $ mkTracedSyn p of
                ParallelSyntax ast _ ->
                    BeamSqlBackendWindowFrameSyntax' $
                        ParallelSyntax
                            ast
                            (fromBeamSqlBackendWindowFrameSyntax $ mkSyn p)
    projectSkeleton' contextPredicateProxy _ fn =
        fmap BTWrapper $
            projectSkeleton'
                contextPredicateProxy
                (Proxy
                    @( BeamTraced be
                     , WithExprContext
                        (BeamSqlBackendWindowFrameSyntax' (BeamTraced be))
                     )
                )
                (\contextProxy _ ->
                    fmap
                        unsafeFromSynP
                        (fn contextProxy (Proxy @be))
                )
      where
        unsafeFromSynP mkSyn p =
            BeamSqlBackendWindowFrameSyntax' $
            unsafeFromSyn $
            fromBeamSqlBackendWindowFrameSyntax $
            mkSyn p

unliftQTraced ::
    Q (BeamTraced be) db s a -> Q be db s a
unliftQTraced (Q fm) =
    Q $ doHoist fm
  where
    doHoist :: QM (BeamTraced be) db s x -> QM be db s x
    doHoist = Free.Church.hoistF mapF
    mapF :: QF (BeamTraced be) db s x -> QF be db s x
    mapF (QDistinct nubType qm next) =
        QDistinct
            (fmap getSyn . nubType . unwrapBTWrapper)
            (doHoist (fmap BTWrapper qm))
            (next . unwrapBTWrapper)
    mapF (QAll mkFrom mkTbl mkOn next) =
        QAll
            (\prefix t -> getSyn $ mkFrom prefix t)
            (BTWrapper . mkTbl)
            (fmap (fmap getSyn) . mkOn . unwrapBTWrapper)
            (next . fmap unwrapBTWrapper)
    mapF (QArbitraryJoin qm mkJoin mkOn next) =
        QArbitraryJoin
            (doHoist (fmap BTWrapper qm))
            (\asyn bsyn mbOnSyn ->
                getSyn $
                    mkJoin
                        (unsafeFromSyn asyn)
                        (unsafeFromSyn bsyn)
                        (fmap unsafeFromSyn mbOnSyn)
            )
            (fmap (fmap getSyn) . mkOn . unwrapBTWrapper)
            (next . unwrapBTWrapper)
    mapF (QTwoWayJoin qm qn mkJoin mkOn next) =
        QTwoWayJoin
            (doHoist (fmap BTWrapper qm))
            (doHoist (fmap BTWrapper qn))
            (\asyn bsyn mbOnSyn ->
                getSyn $
                    mkJoin
                        (unsafeFromSyn asyn)
                        (unsafeFromSyn bsyn)
                        (fmap unsafeFromSyn mbOnSyn)
            )
            (\(BTWrapper a, BTWrapper b) ->
                fmap (fmap getSyn) $ mkOn (a, b)
            )
            (\(BTWrapper a, BTWrapper b) ->
                next (a, b)
            )
    mapF (QSubSelect qm next) =
        QSubSelect
            (doHoist (fmap BTWrapper qm))
            (next . unwrapBTWrapper)
    mapF (QGuard tracedSyn next) =
        QGuard
            (fmap getSyn tracedSyn)
            next
    mapF (QLimit i qm next) =
        QLimit
            i
            (doHoist (fmap BTWrapper qm))
            (next . unwrapBTWrapper)
    mapF (QOffset i qm next) =
        QOffset
            i
            (doHoist (fmap BTWrapper qm))
            (next . unwrapBTWrapper)
    mapF (QSetOp mkOp qm qn next) =
        QSetOp
            (\asyn bsyn ->
                getSyn $
                    mkOp
                        (unsafeFromSyn asyn)
                        (unsafeFromSyn bsyn)
            )
            (doHoist (fmap BTWrapper qm))
            (doHoist (fmap BTWrapper qn))
            (next . unwrapBTWrapper)
    mapF (QOrderBy mkOrder qm next) =
        QOrderBy
            (fmap (map getSyn) . mkOrder . unwrapBTWrapper)
            (doHoist (fmap BTWrapper qm))
            (next . unwrapBTWrapper)
    mapF (QWindowOver mkWindow mkProj qm next) =
        QWindowOver
            (BTWrapper . mkWindow . unwrapBTWrapper)
            (\(BTWrapper r) (BTWrapper w) ->
                BTWrapper $ mkProj r w
            )
            (doHoist (fmap BTWrapper qm))
            (next . unwrapBTWrapper)
    mapF (QAggregate mkAgg qm next) =
        QAggregate
            (\(BTWrapper a) p ->
                case mkAgg a p of
                    (mbTracedSyn, g) ->
                        (fmap getSyn mbTracedSyn, BTWrapper g)
            )
            (doHoist (fmap BTWrapper qm))
            (next . unwrapBTWrapper)
    mapF (QForceSelect mkSelect qm next) =
        QForceSelect
            (\(BTWrapper r) tsyn osynList mbLim mbOff ->
                getSyn $
                    mkSelect
                        r
                        (unsafeFromSyn tsyn)
                        (fmap unsafeFromSyn osynList)
                        mbLim
                        mbOff
            )
            (doHoist (fmap BTWrapper qm))
            (next . unwrapBTWrapper)

-- data RecordingHandle beM = RecordingHandle
    -- { recordReturningManyStart :: Aeson.Value -> IO ()
    -- , recordReturningManyNext :: Aeson.Value -> IO ()
    -- , recordReturningManyEnd :: IO ()
    -- , recordReturningManyException :: IO ()
    -- , recordNoReturnQuery ::
        -- Aeson.Value -> Maybe () -> IO ()
    -- , recordReturningOneQuery ::
        -- Aeson.Value -> Maybe (Maybe Aeson.Value) -> IO ()
    -- , recordReturningListQuery ::
        -- Aeson.Value -> Maybe [Aeson.Value] -> IO ()
    -- , recordingRun :: forall a. beM a -> IO a
    -- }

-- recordingHandle :: (forall a. beM a -> IO a) -> RecordingHandle beM
-- recordingHandle runner = RecordingHandle
    -- { recordReturningManyStart = \v ->
        -- putStrLn $ "recordReturningManyStart\n" <> showYaml v
    -- , recordReturningManyNext = \v ->
        -- putStrLn $ "recordReturningManyNext\n" <> showYaml v
    -- , recordReturningManyEnd =
        -- putStrLn $ "recordReturningManyEnd"
    -- , recordReturningManyException =
        -- putStrLn $ "recordReturningManyException"
    -- , recordNoReturnQuery = \v r ->
        -- putStrLn $ "recordNoReturnQuery\n" <> showYaml v <> "\n" <> showYaml r
    -- , recordReturningOneQuery = \v r ->
        -- putStrLn $ "recordReturningOneQuery\n" <> showYaml v <> "\n" <> showYaml r
    -- , recordReturningListQuery = \v r ->
        -- putStrLn $ "recordReturningListQuery\n" <> showYaml v <> "\n" <> showYaml r
    -- , recordingRun =
        -- runner
    -- }

newtype BeamRunner beM m = BeamRunner
    { execBeamRunner :: forall t. beM t -> m t
    }

newtype BeamT beM m a = BeamT
    { runBeamT :: (forall t. beM t -> m t) -> m a
    }
    deriving
        ( Functor, Applicative, Monad
        , MonadIO, MonadThrow, MonadCatch, MonadMask
        )
        via (ReaderT (BeamRunner beM m) m)

instance Reducible.Reducible (BeamT beM m) where
    type Reduced (BeamT beM m) = ReaderT (BeamRunner beM m) m

class MonadBeamTraced be m | m -> be where
    runSelectReturningListTraced ::
        (FromBackendRow be a, Aeson.ToJSON a, Aeson.FromJSON a) =>
        SqlSelect (BeamTraced be) a -> m [a]
    runSelectReturningOneTraced ::
        (FromBackendRow be a, Aeson.ToJSON a, Aeson.FromJSON a) =>
        SqlSelect (BeamTraced be) a -> m (Maybe a)

instance
    ( MonadBeam be beM
    , BeamSqlBackend be
    ) =>
    MonadBeamTraced be (BeamT beM m)
  where
    runSelectReturningListTraced (SqlSelect (ParallelSyntax _ syn)) =
        BeamT $ \runner ->
            runner $ runSelectReturningList (SqlSelect syn)
    runSelectReturningOneTraced (SqlSelect (ParallelSyntax _ syn)) =
        BeamT $ \runner ->
            runner $ runSelectReturningOne (SqlSelect syn)

instance
    ( MonadBeamTraced be m
    , Record.MonadRecorderBase m
    ) =>
    MonadBeamTraced be (Record.RecordT m)
  where
    runSelectReturningListTraced stat@(SqlSelect (ParallelSyntax ast _)) =
        Record.RecordT $ \rt -> do
            Record.atExit
                (runSelectReturningListTraced stat)
                (\ec -> do
                    liftIO $ Record.recordTraceRecordEvent rt "MonadBeamTraced"
                        (Just
                            ( Aeson.toJSON ("runSelectReturningListTraced" :: Text.Text, ast)
                            , Aeson.toJSON (Record.exitCaseToResult () [] ec)
                            )
                        )
                        (Just (Aeson.toJSON ast))
                        (Just "")
                )
    runSelectReturningOneTraced stat@(SqlSelect (ParallelSyntax ast _)) =
        Record.RecordT $ \rt -> do
            Record.atExit
                (runSelectReturningOneTraced stat)
                (\ec -> do
                    liftIO $ Record.recordTraceRecordEvent rt "MonadBeamTraced"
                        (Just
                            ( Aeson.toJSON ("runSelectReturningOneTraced" :: Text.Text, ast)
                            , Aeson.toJSON (Record.exitCaseToResult () [] ec)
                            )
                        )
                        (Just (Aeson.toJSON ast))
                        (Just "")
                )

newtype BeamReplayT (beM :: * -> *) m a = BeamReplayT
    { runBeamReplayT :: m a
    }
    deriving newtype
        ( Functor, Applicative, Monad
        , MonadIO, MonadThrow, MonadCatch, MonadMask
        )

instance
    ( MonadBeam be beM
    , BeamSqlBackend be
    , Record.MonadReplay m
    ) =>
    MonadBeamTraced be (BeamReplayT beM m)
  where
    runSelectReturningListTraced (SqlSelect (ParallelSyntax ast _)) =
        BeamReplayT $ do
            vj <- fmap Record.fromJustI $ Record.baseReplayTraceEvent "MonadBeamTraced"
                (Record.JustI (Aeson.toJSON ("runSelectReturningListTraced" :: Text.Text, ast)))
                (Just (Aeson.toJSON ast))
                (Just "")
            case Aeson.fromJSON vj of
                Aeson.Error e -> Record.baseReplayIO $ fail e
                Aeson.Success (Record.Success v) -> pure v
                Aeson.Success (Record.Failure ()) -> Record.baseReplayIO $ fail "()"
    runSelectReturningOneTraced (SqlSelect (ParallelSyntax ast _)) =
        BeamReplayT $ do
            vj <- fmap Record.fromJustI $ Record.baseReplayTraceEvent "MonadBeamTraced"
                (Record.JustI (Aeson.toJSON ("runSelectReturningOneTraced" :: Text.Text, ast)))
                (Just (Aeson.toJSON ast))
                (Just "")
            case Aeson.fromJSON vj of
                Aeson.Error e -> Record.baseReplayIO $ fail e
                Aeson.Success (Record.Success v) -> pure v
                Aeson.Success (Record.Failure ()) -> Record.baseReplayIO $ fail "()"

foo ::
    (MonadBeamTraced Postgres.Postgres m) =>
    m [(Integer, String, Bool)]
foo = do
    runSelectReturningListTraced $ select $ do
        values_
            [ (valGenExpr 1, valGenExpr "a", valGenExpr False)
            , (valGenExpr 2, valGenExpr "b", valGenExpr True)
            ]
  where
    valGenExpr ::
        (HasSqlValueSyntax
            (Sql92ExpressionValueSyntax
                (Sql92SelectTableExpressionSyntax
                    (Sql92SelectSelectTableSyntax
                        (Sql92SelectSyntax
                            (BeamSqlBackendSyntax be)))))
            a) =>
        (BeamSqlBackend be) =>
        a ->
        QGenExpr ctx be s a
    valGenExpr = val_

bar :: IO [(Integer, String, Bool)]
bar = do
    bracket (PS.connectPostgreSQL "") PS.close $ \conn -> do
        runBeamT
            (Record.recording foo)
            (Postgres.runBeamPostgresDebug putStrLn conn)

test2 :: IO ()
test2 = do
    tttv <- newTVarIO Map.empty
    etv <- newTVarIO Seq.empty
    let rt = Record.ReplayContext
            { Record.replayContextTimeTableTVar = tttv
            , Record.replayContextSyncPoints = Map.fromList
                [ (("MonadBeamTraced", ""), IntSet.fromList [1])
                ]
            , Record.replayContextRequestMap = Map.fromList
                [ ( ( "MonadBeamTraced"
                    , Aeson.toJSON ("runSelectReturningListTraced" :: Text.Text, selectStatement)
                    )
                  , Map.fromList
                    [ (1, selectResult)
                    ]
                  )
                ]
            , Record.replayContextEffectListTVar = etv
            }
    er <- flip Record.runReplay rt $ do
        runBeamReplayT @Postgres.Pg $ do
            foo
    atomically (readTVar etv) >>= BS.Char8.putStrLn . Record.renderYaml
    print er

selectResult :: Aeson.Value
selectResult = [Yaml.TH.yamlQQ|
    contents:
    - - 1
      - a
      - false
    - - 2
      - b
      - true
    - - 3
      - c
      - true
    tag: Success
    |]

selectStatement :: SyntaxAST
selectStatement = SyntaxAST [Yaml.TH.yamlQQ|
    selectStmt:
    - selectTableStmt:
      - null
      - projExprs:
        - - fieldE:
              qualifiedField:
              - t0
              - res0
          - res0
        - - fieldE:
              qualifiedField:
              - t0
              - res1
          - res1
        - - fieldE:
              qualifiedField:
              - t0
              - res2
          - res2
      - fromTable:
        - tableFromValues:
          - - valueE: 1
            - valueE: a
            - valueE: false
          - - valueE: 2
            - valueE: b
            - valueE: true
        - - t0
          - - res0
            - res1
            - res2
      - null
      - null
      - null
    - []
    - null
    - null
    |]
          -- - 0
    -- - - selectStmt:
        -- - selectTableStmt:
          -- - null
          -- - projExprs:
            -- - - fieldE:
                  -- qualifiedField:
                  -- - t0
                  -- - res0
              -- - res0
            -- - - fieldE:
                  -- qualifiedField:
                  -- - t0
                  -- - res1
              -- - res1
            -- - - fieldE:
                  -- qualifiedField:
                  -- - t0
                  -- - res2
              -- - res2
          -- - fromTable:
            -- - tableFromValues:
              -- - - valueE: 1
                -- - valueE: a
                -- - valueE: false
              -- - - valueE: 2
                -- - valueE: b
                -- - valueE: true
            -- - - t0
              -- - - res0
                -- - res1
                -- - res2
          -- - null
          -- - null
          -- - null
        -- - []
        -- - null
        -- - null
    -- |]