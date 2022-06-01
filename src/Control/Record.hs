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
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Record
    -- (
      -- Reducible (..)
    -- , applyLifting
    -- , Lifting (..)
    -- , MonadLifting (..)
    -- )
where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Foldable
import Data.IORef
import Data.Maybe
import Data.Sequence (Seq (..))
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics
import qualified Control.Monad.Catch.Pure as Catch
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Yaml.Pretty as Yaml.Pretty



data MaybeI t b where
    NothingI :: MaybeI t 'False
    JustI :: t -> MaybeI t 'True



renderYaml :: (Aeson.ToJSON a) => a -> BS.Char8.ByteString
renderYaml =
    Yaml.Pretty.encodePretty yamlConfig
  where
    yamlConfig =
        Yaml.Pretty.setConfCompare compare $
        Yaml.Pretty.setConfDropNull True $
        Yaml.Pretty.defConfig




newtype ActionId = ActionId Int
    deriving newtype (Aeson.ToJSON, Aeson.FromJSON)

newtype ActionType = ActionType Text
    deriving newtype (IsString, Aeson.ToJSON, Aeson.FromJSON)

data TraceRequest = TraceRequest
    { traceRequestDomain :: Text
    , traceRequestTime :: Int
    , traceRequestBody :: Aeson.Value
    , traceRequestResult :: Aeson.Value
    }
    deriving (Generic)

instance Aeson.ToJSON TraceRequest

instance Aeson.FromJSON TraceRequest

data TraceEffect = TraceEffect
    { traceEffectBody :: Aeson.Value
    }
    deriving (Generic)

instance Aeson.ToJSON TraceEffect

instance Aeson.FromJSON TraceEffect

data TraceSyncPoint = TraceSyncPoint
    { traceSyncPointDomain :: Text
    , traceSyncPointTime :: Int
    , traceSyncPointMarker :: Aeson.Value
    }
    deriving (Generic)

instance Aeson.ToJSON TraceSyncPoint

instance Aeson.FromJSON TraceSyncPoint



data RecorderContext = RecorderContext

newtype RecorderBaseT m a = RecorderBaseT
    { runRecorderBaseT :: RecorderContext -> m a
    }
    deriving (Functor, Applicative, Monad, MonadIO)
        via (ReaderT RecorderContext m)
    deriving (MonadTrans)
        via (ReaderT RecorderContext)



data TraceEvent
    = TraceEventRequest Text Aeson.Value Aeson.Value
    | TraceEventEffect Aeson.Value
    | TraceEventSyncPoint Text Aeson.Value
    deriving (Generic)

instance Aeson.ToJSON TraceEvent

instance Aeson.FromJSON TraceEvent



data TraceData = TraceData
    { traceDataRequests :: Map.Map (Text, Aeson.Value) (Seq (Int, Aeson.Value))
    , traceDataEffects :: Seq Aeson.Value
    , traceDataSyncPoints :: Map.Map Text (Seq (Int, Aeson.Value))
    }
    deriving (Generic)

instance Aeson.ToJSON TraceData

instance Aeson.FromJSON TraceData



-- constructTraceData ::
    -- Seq TraceEvent ->
    -- IO TraceData
-- constructTraceData = fmap snd . foldlM iter (Map.empty, initTraceData)
  -- where
    -- initTraceData = TraceData Map.empty Seq.empty Map.empty
    -- iter (timeTable, traceData) event = case event of
        -- TraceEventRequest domain rkey rvalue -> do
            -- let time =
                    -- fromMaybe 0 $
                    -- Map.lookup domain timeTable
            -- newReqMap <-
                    -- Map.alterF
                        -- (appendRequest domain time rkey rvalue)
                        -- (domain, rkey)
                        -- (traceDataRequests traceData)
            -- pure (timeTable, traceData {traceDataRequests = newReqMap})
        -- TraceEventEffect value -> do
            -- let newEffectSeq =
                    -- traceDataEffects traceData :|> value
            -- pure (timeTable, traceData {traceDataEffects = newEffectSeq})
        -- TraceEventSyncPoint domain marker -> do
            -- let oldTime =
                    -- fromMaybe 0 $
                    -- Map.lookup domain timeTable
            -- let newTime = oldTime + 1
            -- let newSyncMap =
                    -- Map.insertWith
                        -- (flip (<>))
                        -- domain
                        -- (Seq.singleton (newTime, marker))
                        -- (traceDataSyncPoints traceData)
            -- let newTimeTable =
                    -- Map.insert domain newTime timeTable
            -- pure (newTimeTable, traceData {traceDataSyncPoints = newSyncMap})
    -- appendRequest ::
        -- Text -> Int -> Aeson.Value -> Aeson.Value ->
        -- Maybe (Seq (Int, Aeson.Value)) ->
        -- IO (Maybe (Seq (Int, Aeson.Value)))
    -- appendRequest _ time _ rvalue Nothing = do
        -- pure (Just (Seq.singleton (time, rvalue)))
    -- appendRequest domain time rkey rvalue (Just oldList) = do
        -- case oldList of
            -- _ :|> (prevTime, prevValue) | prevTime == time -> do
                -- unless (prevValue == rvalue) $ do
                    -- putStrLn $
                        -- "Inconsistent trace, ignoring value for " <>
                        -- Text.unpack domain <> "-" <> show time <> "\n" <>
                        -- BS.Char8.unpack (renderYaml (rkey, rvalue))
                -- pure (Just oldList)
            -- _ -> do
                -- pure (Just (oldList :|> (time, rvalue)))



data RecordTrace = RecordTrace
    { recordTraceTimeTableTVar ::
        TVar (Map.Map Text Int)
    , recordTraceSyncPointSetTVar ::
        TVar (Set.Set (Text, Aeson.Value, Int))
    , recordTraceRequestMapTVar ::
        TVar (Map.Map (Text, Aeson.Value, Int) Aeson.Value)
    , recordTraceEffectListTVar ::
        TVar (Seq Aeson.Value)
    }



baseRecordTraceEvent ::
    (MonadRecorderBase m) =>
    RecordTrace ->
    Text ->
    Maybe (Aeson.Value, Aeson.Value) ->
    Maybe Aeson.Value ->
    Maybe Aeson.Value ->
    m ()
baseRecordTraceEvent rt domain mbKeyValue mbEffect mbMarker =
    liftIO $ do
        mbBadValueMessage <- atomically $ do
            badValueMessageTVar <- newTVar Nothing
            oldTimeTable <- readTVar (recordTraceTimeTableTVar rt)
            let oldTime =
                    fromMaybe 0 $
                    Map.lookup domain oldTimeTable
            oldReqMap <- readTVar (recordTraceRequestMapTVar rt)
            forM_ mbKeyValue $ \(rkey, rvalue) -> do
                let (isBadReqValue, newReqMap) =
                        Map.alterF @((,) Bool)
                            (maybe
                                (False, Just rvalue)
                                (\oldValue -> (oldValue /= rvalue, Just rvalue))
                            )
                            (domain, rkey, oldTime)
                            oldReqMap
                when isBadReqValue $ do
                    writeTVar badValueMessageTVar $
                        Just $
                            "Inconsistent trace, ignoring value for " <>
                            Text.unpack domain <> "@" <> show oldTime <> "\n" <>
                            BS.Char8.unpack (renderYaml (rkey, rvalue))
                writeTVar (recordTraceRequestMapTVar rt) $! newReqMap
            forM_ mbEffect $ \effect -> do
                modifyTVar' (recordTraceEffectListTVar rt)
                    (:|> effect)
            forM_ mbMarker $ \marker -> do
                let newTime = oldTime + 1
                modifyTVar' (recordTraceSyncPointSetTVar rt)
                    (Set.insert (domain, marker, newTime))
                writeTVar (recordTraceTimeTableTVar rt) $!
                    Map.insert domain newTime oldTimeTable
            readTVar badValueMessageTVar
        forM_ mbBadValueMessage $ \message ->
            putStrLn message



class (MonadIO m) => MonadRecorderBase m where
    baseWithRecordTrace ::
        (RecordTrace -> m a) ->
        m a
    -- baseRecordTraceEvent ::
        -- RecordTrace ->
        -- TraceEvent ->
        -- m ()



instance (MonadRecorderBase m) => MonadRecorderBase (ReaderT r m) where
    baseWithRecordTrace inner = ReaderT $ \context ->
        baseWithRecordTrace (\rt -> runReaderT (inner rt) context)
    -- baseRecordTraceEvent rt event =
        -- lift $ baseRecordTraceEvent rt event

instance MonadRecorderBase IO where
    baseWithRecordTrace inner =
        bracket acquire release inner
      where
        acquire :: IO RecordTrace
        acquire =
            RecordTrace
                <$> newTVarIO Map.empty
                <*> newTVarIO Set.empty
                <*> newTVarIO Map.empty
                <*> newTVarIO Seq.empty
        release rt = do
            (tt, sps, rm, el) <- atomically $
                (,,,)
                    <$> readTVar (recordTraceTimeTableTVar rt)
                    <*> readTVar (recordTraceSyncPointSetTVar rt)
                    <*> readTVar (recordTraceRequestMapTVar rt)
                    <*> readTVar (recordTraceEffectListTVar rt)
            BS.Char8.putStrLn $ renderYaml (tt, sps, rm, el)
            -- eventLog <- atomically $ readTVar (recordTraceEventLogTVar rt)
            -- traceData <- constructTraceData eventLog
            -- BS.Char8.putStrLn $ renderYaml eventLog
            -- BS.Char8.putStrLn $ renderYaml traceData
            pure ()
    -- baseRecordTraceEvent rt event =
        -- atomically $ do
            -- modifyTVar' (recordTraceEventLogTVar rt) (:|> event)
            -- pure ()



newtype RecordT m a = RecordT
    { runRecordT :: RecordTrace -> m a
    }
    deriving (Functor, Applicative, Monad, MonadIO)
        via (ReaderT RecordTrace m)



recording ::
    (MonadRecorderBase m) =>
    RecordT m a ->
    m a
recording inner = do
    baseWithRecordTrace $ \rt ->
        runRecordT inner rt



newtype RecordActionT m a = RecordActionT
    { runRecordActionT :: (RecordTrace, ActionId) -> m a
    }
    deriving (Functor, Applicative, Monad, MonadIO)
        via (ReaderT (RecordTrace, ActionId) m)
    deriving (MonadTrans)
        via (ReaderT (RecordTrace, ActionId))



class Monad m => MonadKeyValue m where
    getKV ::
        Text ->
        m (Maybe Text)
    setKV ::
        Text ->
        Text ->
        m ()
    traverseKVSatisfying ::
        (Text -> Bool) ->
        b ->
        (b -> Text -> Text -> m (b, Text)) ->
        m b



instance (MonadIO m, MonadMask m) => MonadKeyValue (ReaderT (TMVar (Map.Map Text Text)) m) where
    getKV k = ReaderT $ \tmvar -> do
        liftIO $ atomically $ do
            Map.lookup k <$> readTMVar tmvar
    setKV k v = ReaderT $ \tmvar -> do
        liftIO $ atomically $ do
            ss <- takeTMVar tmvar
            putTMVar tmvar (Map.insert k v ss)
    traverseKVSatisfying cond b0 fn = do
        ((b, _), ()) <- generalBracket acquire release use
        pure b
      where
        acquire = ReaderT $ \tmvar -> do
            liftIO $ atomically $ do
                takeTMVar tmvar
        release oldSS exitCase = ReaderT $ \tmvar -> do
            liftIO $ atomically $ do
                case exitCase of
                    ExitCaseSuccess (_, newSS) -> putTMVar tmvar newSS
                    _ -> putTMVar tmvar oldSS
        use oldSS =
            go b0 oldSS (Map.toList oldSS)
        go b ss [] = do
            pure (b, ss)
        go b1 ss ((k, v1) : rest)
            | cond k = do
                (b2, v2) <- fn b1 k v1
                go b2 (Map.insert k v2 ss) rest
            | otherwise = do
                go b1 ss rest



instance (MonadRecorderBase m, MonadKeyValue m) => MonadKeyValue (RecordT m) where
    getKV k = RecordT $ \rt -> do
        v <- getKV k
        baseRecordTraceEvent rt "MonadKeyValue"
            (Just (Aeson.toJSON ["getKV", k], Aeson.toJSON v))
            Nothing
            Nothing
        pure v
    setKV k v = RecordT $ \rt -> do
        baseRecordTraceEvent rt "MonadKeyValue"
            Nothing
            (Just (Aeson.toJSON ("setKV" :: Text, k, v)))
            (Just "setKV")
        setKV k v
    traverseKVSatisfying cond b0 fn = RecordT $ \rt -> do
        raccum <- liftIO $ newIORef Seq.empty
        waccum <- liftIO $ newIORef Map.empty
        br <- traverseKVSatisfying cond b0 $ \b1 k v1 -> do
            liftIO $ modifyIORef' raccum (:|> (k, v1))
            (b2, v2) <- runRecordT (fn b1 k v1) rt
            liftIO $ modifyIORef' waccum (Map.insert k v2)
            pure (b2, v2)
        rseq <- liftIO $ readIORef raccum
        wmap <- liftIO $ readIORef waccum
        baseRecordTraceEvent rt "MonadKeyValue"
            (Just ("traverseKVSatisfying", Aeson.toJSON rseq))
            (Just (Aeson.toJSON ("traverseKVSatisfying" :: Text, wmap)))
            (Just "traverseKVSatisfying")
        pure br



test :: IO ()
test = do
    sstmvar <- newTMVarIO (Map.empty :: Map.Map Text Text)
    flip runReaderT sstmvar $ do
        recording $ do
            func



func :: MonadKeyValue m => m ()
func = do
    setKV "a" "b"
    setKV "c" "d"
    mx <- getKV "a"
    setKV "e" $ maybe "" id mx
    traverseKVSatisfying (const True) () $ \() k v -> do
        pure ((), k <> v)



data ReplayError
    = ReplayErrorNoRequest Text Aeson.Value
    deriving (Show)

instance Exception ReplayError



data ReplayContext = ReplayContext
    { replayContextTimeTableTVar ::
        TVar (Map.Map Text Int)
    , replayContextSyncPoints ::
        Map.Map (Text, Aeson.Value) IntSet.IntSet
    , replayContextRequestMap ::
        Map.Map (Text, Aeson.Value) (Map.Map Int Aeson.Value)
    , replayContextEffectListTVar ::
        TVar (Seq Aeson.Value)
    }



baseReplayTraceEvent ::
    ReplayContext ->
    Text ->
    MaybeI Aeson.Value hasreq ->
    Maybe Aeson.Value ->
    Maybe Aeson.Value ->
    IO (MaybeI Aeson.Value hasreq)
baseReplayTraceEvent rt domain mbiKey mbEffect mbMarker =
    atomically $ do
        oldTimeTable <- readTVar (replayContextTimeTableTVar rt)
        let oldTime =
                fromMaybe 0 $
                Map.lookup domain oldTimeTable
        forM_ mbMarker $ \marker -> do
            let mbNewTime = do
                    timeSet <-
                        Map.lookup
                            (domain, marker)
                            (replayContextSyncPoints rt)
                    IntSet.lookupGT oldTime timeSet
            forM_ mbNewTime $ \newTime -> do
                writeTVar (replayContextTimeTableTVar rt) $!
                    Map.insert domain newTime oldTimeTable
        forM_ mbEffect $ \effect -> do
            modifyTVar' (replayContextEffectListTVar rt)
                (:|> effect)
        case mbiKey of
            NothingI -> pure NothingI
            JustI rkey -> do
                let mbSelectedResp = do
                        responses <-
                            Map.lookup
                                (domain, rkey)
                                (replayContextRequestMap rt)
                        Map.lookupLE oldTime responses <|>
                            Map.lookupMin responses
                case mbSelectedResp of
                    Nothing ->
                        throwSTM $ ReplayErrorNoRequest domain rkey
                    Just (_, selectedResp) ->
                        pure (JustI selectedResp)



newtype Replay a = Replay
    { runReplay ::
        ReplayContext -> IO (Either SomeException a)
    }
    deriving (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadMask)
        via (ReaderT ReplayContext (Catch.CatchT IO))



instance MonadKeyValue Replay where
    getKV k = Replay $ \rt -> do
        JustI vj <- baseReplayTraceEvent rt "MonadKeyValue"
            (JustI (Aeson.toJSON ["getKV", k]))
            Nothing
            Nothing
        case Aeson.fromJSON vj of
            Aeson.Success v -> pure (Right v)
            Aeson.Error e -> fail e
    setKV k v = Replay $ \rt -> do
        NothingI <- baseReplayTraceEvent rt "MonadKeyValue"
            NothingI
            (Just (Aeson.toJSON ("setKV" :: Text, k, v)))
            (Just "setKV")
        pure (Right ())
    traverseKVSatisfying _ b0 fn = Replay $ \rt -> do
        JustI vsj <- baseReplayTraceEvent rt "MonadKeyValue"
            (JustI "traverseKVSatisfying")
            Nothing
            Nothing
        vs <- case Aeson.fromJSON vsj of
            Aeson.Success vs -> pure (vs :: Seq (Text, Text))
            Aeson.Error e -> fail e
        waccum <- newIORef Map.empty
        br <- flip runReplay rt $
            foldlM
                (\b1 (k, v1) -> do
                    (b2, v2) <- fn b1 k v1
                    Replay $ \_ -> Right <$> modifyIORef' waccum (Map.insert k v2)
                    pure b2
                )
                b0
                vs
        wmap <- readIORef waccum
        NothingI <- baseReplayTraceEvent rt "MonadKeyValue"
            NothingI
            (Just (Aeson.toJSON ("traverseKVSatisfying" :: Text, wmap)))
            Nothing
        pure br
    -- traverseKVSatisfying cond b0 fn = RecordT $ \rt -> do
        -- raccum <- liftIO $ newIORef Seq.empty
        -- waccum <- liftIO $ newIORef Map.empty
        -- br <- traverseKVSatisfying cond b0 $ \b1 k v1 -> do
            -- liftIO $ modifyIORef' raccum (:|> (k, v1))
            -- (b2, v2) <- runRecordT (fn b1 k v1) rt
            -- liftIO $ modifyIORef' waccum (Map.insert k v2)
            -- pure (b2, v2)
        -- rseq <- liftIO $ readIORef raccum
        -- wseq <- liftIO $ readIORef waccum
        -- baseRecordTraceEvent rt "MonadKeyValue"
            -- (Just ("traverseKVSatisfying", Aeson.toJSON rseq))
            -- (Just (Aeson.toJSON ("traverseKVSatisfying" :: Text, wseq)))
            -- (Just "traverseKVSatisfying")
        -- pure br



test2 :: IO ()
test2 = do
    tttv <- newTVarIO Map.empty
    etv <- newTVarIO Seq.empty
    let rt = ReplayContext
            { replayContextTimeTableTVar = tttv
            , replayContextSyncPoints = Map.fromList
                [ (("MonadKeyValue", "setKV"), IntSet.fromList [1, 2, 3])
                , (("MonadKeyValue", "traverseKVSatisfying"), IntSet.fromList [4])
                ]
            , replayContextRequestMap = Map.fromList
                [ ( ("MonadKeyValue", Aeson.toJSON ("getKV" :: Text, "a" :: Text))
                  , Map.fromList
                    [ (2, "b")
                    ]
                  )
                , ( ("MonadKeyValue", "traverseKVSatisfying")
                  , Map.fromList
                    [ (3, Aeson.toJSON [("a" :: Text, "b" :: Text), ("c", "d"), ("e", "b")])
                    ]
                  )
                ]
            , replayContextEffectListTVar = etv
            }
    er <- flip runReplay rt $ do
        func
    atomically (readTVar etv) >>= BS.Char8.putStrLn . renderYaml
    print er



