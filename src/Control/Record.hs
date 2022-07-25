{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Functor.Identity
import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.Sequence (Seq (..))
import Data.String (IsString)
import Data.Text (Text)
import Data.Void
import GHC.Generics
import qualified Type.Reflection as Type
import qualified Control.Monad.Catch.Pure as Catch
import qualified Control.Reducible as Reducible
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.ByteString.Lazy.Char8 as BS.Lazy.Char8
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Yaml.Pretty as Yaml.Pretty
import qualified Data.Yaml.TH as Yaml.TH



atExit ::
    (MonadMask m) =>
    m a ->
    (ExitCase a -> m b) ->
    m a
atExit act handler =
    fmap fst $
        generalBracket
            (pure ())
            (\() ec -> handler ec)
            (\() -> act)



data Result e a
    = Failure e
    | Success a
    deriving (Show, Eq, Ord, Generic)

instance (Aeson.ToJSON e, Aeson.ToJSON a) => Aeson.ToJSON (Result e a) where
    toJSON = Aeson.genericToJSON (aesonOptions 0)
    toEncoding = Aeson.genericToEncoding (aesonOptions 0)

instance (Aeson.FromJSON e, Aeson.FromJSON a) => Aeson.FromJSON (Result e a) where
    parseJSON = Aeson.genericParseJSON (aesonOptions 0)



data TextTagged a = TextTagged Text a
    deriving (Show, Eq, Ord, Generic)

instance (Aeson.ToJSON a) => Aeson.ToJSON (TextTagged a) where
    toJSON (TextTagged tag value) =
        Aeson.object [tag Aeson..= value]
    toEncoding (TextTagged tag value) =
        Aeson.pairs (tag Aeson..= value)

instance (Aeson.FromJSON a) => Aeson.FromJSON (TextTagged a) where
    parseJSON = Aeson.withObject "TextTagged" $ \obj -> do
        case HashMap.toList obj of
            [(key, _)] -> do
                value <- obj Aeson..: key
                pure (TextTagged key value)
            _ -> fail "expected an object with single field"



exitCaseToResult ::
    e ->
    [Handler Identity e] ->
    ExitCase a ->
    Result e a
exitCaseToResult defaultFail handlers exitCase =
    case exitCase of
        ExitCaseSuccess x -> Success x
        ExitCaseException someEx -> Failure (go handlers)
          where
            go [] = defaultFail
            go (Handler fn : others) =
                case fromException someEx of
                    Just ex -> runIdentity $ fn ex
                    Nothing -> go others
        ExitCaseAbort -> Failure defaultFail



data MaybeI t b where
    NothingI :: MaybeI t 'False
    JustI :: t -> MaybeI t 'True

fromNothingI :: MaybeI t 'False -> ()
fromNothingI NothingI = ()

fromJustI :: MaybeI t 'True -> t
fromJustI (JustI x) = x



renderYaml :: (Aeson.ToJSON a) => a -> BS.Char8.ByteString
renderYaml =
    Yaml.Pretty.encodePretty yamlConfig
  where
    yamlConfig =
        Yaml.Pretty.setConfCompare compare $
        Yaml.Pretty.defConfig



aesonOptions :: Int -> Aeson.Options
aesonOptions conPrefixLen = Aeson.defaultOptions
    { Aeson.sumEncoding = Aeson.ObjectWithSingleField
    , Aeson.constructorTagModifier = \ct ->
        lowerHead $ List.drop conPrefixLen ct
    }
  where
    lowerHead [] = []
    lowerHead (c : cs)
        | Char.toLower c == c =
            c : cs
        | otherwise =
            Char.toLower c : lowerHead cs




-- data TraceEventRequest = TraceEventRequest
    -- { traceRequestDomain :: Text
    -- , traceRequestTime :: Int
    -- , traceRequestBody :: Aeson.Value
    -- , traceRequestResult :: Aeson.Value
    -- }
    -- deriving (Generic)

-- instance Aeson.ToJSON TraceEventRequest

-- instance Aeson.FromJSON TraceEventRequest

-- data TraceEventEffect = TraceEventEffect
    -- { traceEffectBody :: Aeson.Value
    -- }
    -- deriving (Generic)

-- instance Aeson.ToJSON TraceEventEffect

-- instance Aeson.FromJSON TraceEventEffect

-- data TraceEventSyncPoint = TraceEventSyncPoint
    -- { traceSyncPointDomain :: Text
    -- , traceSyncPointTime :: Int
    -- , traceSyncPointMarker :: Aeson.Value
    -- }
    -- deriving (Generic)

-- instance Aeson.ToJSON TraceEventSyncPoint

-- instance Aeson.FromJSON TraceEventSyncPoint



data RecorderContext = RecorderContext

newtype RecorderBaseT m (a :: *) = RecorderBaseT
    { runRecorderBaseT :: RecorderContext -> m a
    }
    deriving (Functor, Applicative, Monad, MonadIO)
        via (ReaderT RecorderContext m)
    deriving (MonadTrans)
        via (ReaderT RecorderContext)



data TraceEvent
    = TraceEventRequest Aeson.Value Aeson.Value
    | TraceEventEffect Aeson.Value
    | TraceEventSyncPoint Aeson.Value
    deriving (Show, Eq, Ord, Generic)

instance Aeson.ToJSON TraceEvent where
    toJSON = Aeson.genericToJSON
        (aesonOptions (Text.length "TraceEvent"))
    toEncoding = Aeson.genericToEncoding
        (aesonOptions (Text.length "TraceEvent"))

instance Aeson.FromJSON TraceEvent where
    parseJSON = Aeson.genericParseJSON
        (aesonOptions (Text.length "TraceEvent"))



-- data TraceData = TraceData
    -- { traceDataRequests :: Map.Map (Text, Aeson.Value) (Seq (Int, Aeson.Value))
    -- , traceDataEffects :: Seq Aeson.Value
    -- , traceDataSyncPoints :: Map.Map Text (Seq (Int, Aeson.Value))
    -- }
    -- deriving (Generic)

-- instance Aeson.ToJSON TraceData

-- instance Aeson.FromJSON TraceData



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



class
    ( Type.Typeable d
    , Ord (TraceDomainRequest d)
    , Eq (TraceDomainEffect d)
    , Ord (TraceDomainSyncPoint d)
    , Aeson.FromJSON (TraceDomainRequest d)
    , Aeson.FromJSON (TraceDomainError d)
    , Aeson.FromJSON (TraceDomainEffect d)
    , Aeson.FromJSON (TraceDomainSyncPoint d)
    , Aeson.ToJSON (TraceDomainRequest d)
    , Aeson.ToJSON (TraceDomainError d)
    , Aeson.ToJSON (TraceDomainEffect d)
    , Aeson.ToJSON (TraceDomainSyncPoint d)
    ) =>
    TraceDomain (d :: k)
  where
    data TraceDomainRequest d
    data TraceDomainError d
    data TraceDomainEffect d
    data TraceDomainSyncPoint d
    traceDomainErrorFromException ::
        Maybe SomeException ->
        Maybe (TraceDomainError d)
    traceDomainErrorToException ::
        TraceDomainError d ->
        SomeException

domainName :: forall d. (TraceDomain d) => Proxy d -> Text
domainName _ = Text.pack $ show $ Type.typeRep @d

noRequest :: Maybe (TraceDomainRequest d, Void)
noRequest = Nothing



finallyRecordingWith ::
    forall r d m a.
    (TraceDomain d, Aeson.ToJSON r, MonadIO m, MonadMask m) =>
    RecordTrace ->
    TraceDomainRequest d ->
    m a ->
    (a -> m (Maybe r)) ->
    m a
finallyRecordingWith rt key inner encoder = do
    atExit inner $ \ec -> do
        mbResult <-
            case ec of
                ExitCaseSuccess x -> do
                    eiMbValue <- try $ encoder x
                    case eiMbValue of
                        Left ex -> do
                            liftIO $ putStrLn $
                                "Exception while encoding a result: " <>
                                displayException (ex :: SomeException)
                            pure Nothing
                        Right mbValue ->
                            pure (Success <$> mbValue)
                ExitCaseException ex ->
                    pure (Failure <$> traceDomainErrorFromException (Just ex))
                ExitCaseAbort ->
                    pure (Failure <$> traceDomainErrorFromException Nothing)
        case mbResult :: Maybe (Result (TraceDomainError d) r) of
            Nothing -> pure ()
            Just result ->
                liftIO $ recordTraceEventRaw rt $
                    TextTagged (domainName $ Proxy @d) $
                        TraceEventRequest
                            (Aeson.toJSON key)
                            (Aeson.toJSON result)

finallyRecording ::
    forall r d m.
    (TraceDomain d, Aeson.ToJSON r, MonadIO m, MonadMask m) =>
    RecordTrace ->
    TraceDomainRequest d ->
    m r ->
    m r
finallyRecording rt key inner = do
    finallyRecordingWith rt key inner (pure . Just)

recordTraceEventRequestResult ::
    forall d m r.
    (TraceDomain d, Aeson.ToJSON r, MonadIO m) =>
    RecordTrace ->
    TraceDomainRequest d ->
    r ->
    m ()
recordTraceEventRequestResult rt key result = do
    liftIO $ recordTraceEventRaw rt $
        TextTagged (domainName $ Proxy @d) $
            TraceEventRequest
                (Aeson.toJSON key)
                (Aeson.toJSON (Success @Void @r result))

recordTraceEventRequestError ::
    forall d m.
    (TraceDomain d, MonadIO m) =>
    RecordTrace ->
    TraceDomainRequest d ->
    TraceDomainError d ->
    m ()
recordTraceEventRequestError rt key err = do
    liftIO $ recordTraceEventRaw rt $
        TextTagged (domainName $ Proxy @d) $
            TraceEventRequest
                (Aeson.toJSON key)
                (Aeson.toJSON (Failure @(TraceDomainError d) @Void err))

recordTraceEventEffect ::
    forall d m.
    (TraceDomain d, MonadIO m) =>
    RecordTrace ->
    TraceDomainEffect d ->
    m ()
recordTraceEventEffect rt effect = do
    liftIO $ recordTraceEventRaw rt $
        TextTagged (domainName $ Proxy @d) $
            TraceEventEffect (Aeson.toJSON effect)

recordTraceEventSyncPoint ::
    forall d m.
    (TraceDomain d, MonadIO m) =>
    RecordTrace ->
    TraceDomainSyncPoint d ->
    m ()
recordTraceEventSyncPoint rt marker = do
    liftIO $ recordTraceEventRaw rt $
        TextTagged (domainName $ Proxy @d) $
            TraceEventSyncPoint (Aeson.toJSON marker)

-- recordTraceEvent ::
    -- forall d r m.
    -- (TraceDomain d, Aeson.ToJSON r, MonadRecorderBase m) =>
    -- RecordTrace ->
    -- Maybe (TraceDomainRequest d, r) ->
    -- Maybe (TraceDomainEffect d) ->
    -- Maybe (TraceDomainSyncPoint d) ->
    -- m ()
-- recordTraceEvent rt mbDomainKeyValue mbDomainEffect mbDomainMarker = do
    -- liftIO $ recordTraceEventRaw
        -- rt
        -- (Text.pack $ show $ Type.typeRep @d)
        -- (fmap
            -- (\(k, v) -> (Aeson.toJSON k, Aeson.toJSON v))
            -- mbDomainKeyValue
        -- )
        -- (fmap Aeson.toJSON mbDomainEffect)
        -- (fmap Aeson.toJSON mbDomainMarker)



-- data RecordTrace = RecordTrace
    -- { recordTraceTimeTableTVar ::
        -- TVar (Map.Map Text Int)
    -- , recordTraceSyncPointSetTVar ::
        -- TVar (Set.Set (Text, Aeson.Value, Int))
    -- , recordTraceRequestMapTVar ::
        -- TVar (Map.Map (Text, Aeson.Value, Int) Aeson.Value)
    -- , recordTraceEffectListTVar ::
        -- TVar (Seq (Text, Aeson.Value))
    -- }

data RecordTrace = RecordTrace
    { recordTraceCurrentRequestMapTVar ::
        TVar (Map.Map Text (Map.Map Aeson.Value Aeson.Value))
    , recordTraceEventLogTVar ::
        TVar (Seq (TextTagged TraceEvent))
    }



recordTraceEventRaw ::
    RecordTrace ->
    TextTagged TraceEvent ->
    IO ()
recordTraceEventRaw rt traceEvent = do
    mbBadValueMessage <- atomically $ do
        modifyTVar'
            (recordTraceEventLogTVar rt)
            (:|> traceEvent)
        case traceEvent of
            TextTagged domain (TraceEventRequest key newValue) -> do
                oldReqMap <- readTVar
                    (recordTraceCurrentRequestMapTVar rt)
                case Map.lookup key =<< Map.lookup domain oldReqMap of
                    Nothing -> do
                        writeTVar
                            (recordTraceCurrentRequestMapTVar rt) $!
                            (Map.insertWith
                                (<>)
                                domain
                                (Map.singleton key newValue)
                                oldReqMap
                            )
                        pure Nothing
                    Just oldValue
                        | oldValue == newValue ->
                            pure Nothing
                        | otherwise ->
                            pure $ Just $
                                "Inconsistent request results detected." <>
                                "\nMaybe add a sync point to signify " <>
                                "a change of state?" <>
                                "\nDomain: " <>
                                Text.unpack domain <>
                                "\nKey:\n" <>
                                BS.Char8.unpack (renderYaml [key]) <>
                                "Old value:\n" <>
                                BS.Char8.unpack (renderYaml [oldValue]) <>
                                "New value:\n" <>
                                BS.Char8.unpack (renderYaml [newValue])
            TextTagged domain (TraceEventSyncPoint _) -> do
                modifyTVar'
                    (recordTraceCurrentRequestMapTVar rt)
                    (Map.delete domain)
                pure Nothing
            _ ->
                pure Nothing
    forM_ @Maybe mbBadValueMessage $ \message ->
        putStrLn message



-- recordTraceEventRaw ::
    -- RecordTrace ->
    -- Text ->
    -- Maybe (Aeson.Value, Aeson.Value) ->
    -- Maybe Aeson.Value ->
    -- Maybe Aeson.Value ->
    -- IO ()
-- recordTraceEventRaw rt domain mbKeyValue mbEffect mbMarker = do
    -- mbBadValueMessage <- atomically $ do
        -- badValueMessageTVar <- newTVar Nothing
        -- oldTimeTable <- readTVar (recordTraceTimeTableTVar rt)
        -- let oldTime =
                -- fromMaybe 0 $
                -- Map.lookup domain oldTimeTable
        -- oldReqMap <- readTVar (recordTraceRequestMapTVar rt)
        -- forM_ mbKeyValue $ \(rkey, rvalue) -> do
            -- let (isBadReqValue, newReqMap) =
                    -- Map.alterF @((,) Bool)
                        -- (maybe
                            -- (False, Just rvalue)
                            -- (\oldValue -> (oldValue /= rvalue, Just rvalue))
                        -- )
                        -- (domain, rkey, oldTime)
                        -- oldReqMap
            -- when isBadReqValue $ do
                -- writeTVar badValueMessageTVar $
                    -- Just $
                        -- "Inconsistent trace, ignoring value for " <>
                        -- Text.unpack domain <> "@" <> show oldTime <> "\n" <>
                        -- BS.Char8.unpack (renderYaml (rkey, rvalue))
            -- writeTVar (recordTraceRequestMapTVar rt) $! newReqMap
        -- forM_ mbEffect $ \effect -> do
            -- modifyTVar' (recordTraceEffectListTVar rt)
                -- (:|> (domain, effect))
        -- forM_ mbMarker $ \marker -> do
            -- let newTime = oldTime + 1
            -- modifyTVar' (recordTraceSyncPointSetTVar rt)
                -- (Set.insert (domain, marker, newTime))
            -- writeTVar (recordTraceTimeTableTVar rt) $!
                -- Map.insert domain newTime oldTimeTable
        -- readTVar badValueMessageTVar
    -- forM_ mbBadValueMessage $ \message ->
        -- putStrLn message



-- rawRecordTraceEventRequest ::
    -- RecordTrace ->
    -- Text ->
    -- Aeson.Value ->
    -- Aeson.Value ->
    -- IO ()
-- rawRecordTraceEventRequest rt domain rawKey rawValue = do
    -- mbBadValueMessage <- atomically $ do
        -- oldTimeTable <- readTVar (recordTraceTimeTableTVar rt)
        -- let oldTime =
                -- fromMaybe 0 $
                -- Map.lookup domain oldTimeTable
        -- oldReqMap <- readTVar (recordTraceRequestMapTVar rt)
        -- let (isBadReqValue, newReqMap) =
                -- Map.alterF @((,) Bool)
                    -- (maybe
                        -- (False, Just rawValue)
                        -- (\oldValue -> (oldValue /= rawValue, Just rawValue))
                    -- )
                    -- (domain, rawKey, oldTime)
                    -- oldReqMap
        -- let mbBadValueMessage =
                -- if isBadReqValue
                    -- then Just $
                        -- "Inconsistent trace, ignoring value for " <>
                        -- Text.unpack domain <> "@" <> show oldTime <> "\n" <>
                        -- BS.Char8.unpack (renderYaml (rawKey, rawValue))
                    -- else Nothing
        -- writeTVar (recordTraceRequestMapTVar rt) $! newReqMap
        -- pure mbBadValueMessage
    -- forM_ mbBadValueMessage $ \message ->
        -- putStrLn message

-- rawRecordTraceEventEffect ::
    -- RecordTrace ->
    -- Text ->
    -- Aeson.Value ->
    -- IO ()
-- rawRecordTraceEventEffect rt domain rawEffect = do
    -- atomically $ do
        -- modifyTVar' (recordTraceEffectListTVar rt)
            -- (:|> (domain, rawEffect))

-- rawRecordTraceEventSyncPoint ::
    -- RecordTrace ->
    -- Text ->
    -- Aeson.Value ->
    -- IO ()
-- rawRecordTraceEventSyncPoint rt domain rawMarker = do
    -- atomically $ do
        -- oldTimeTable <- readTVar (recordTraceTimeTableTVar rt)
        -- let oldTime =
                -- fromMaybe 0 $
                -- Map.lookup domain oldTimeTable
        -- let newTime = oldTime + 1
        -- modifyTVar' (recordTraceSyncPointSetTVar rt)
            -- (Set.insert (domain, rawMarker, newTime))
        -- writeTVar (recordTraceTimeTableTVar rt) $!
            -- Map.insert domain newTime oldTimeTable




class (MonadIO m, MonadMask m) => MonadRecorderBase m where
    baseWithRecordTrace ::
        (RecordTrace -> m a) ->
        m a



instance (MonadRecorderBase m) => MonadRecorderBase (ReaderT r m) where
    baseWithRecordTrace inner = ReaderT $ \c ->
        baseWithRecordTrace (\rt -> runReaderT (inner rt) c)

instance
    {-# OVERLAPPABLE #-}
    ( Reducible.Reducible m
    , MonadRecorderBase (Reducible.Reduced m)
    , MonadIO m
    , MonadMask m
    ) =>
    MonadRecorderBase m
  where
    baseWithRecordTrace inner = Reducible.fromReduced $
        baseWithRecordTrace (\rt -> Reducible.toReduced (inner rt))

instance MonadRecorderBase IO where
    baseWithRecordTrace inner =
        bracket acquire release inner
      where
        acquire :: IO RecordTrace
        acquire =
            RecordTrace
                <$> newTVarIO Map.empty
                <*> newTVarIO Seq.empty
        release rt = do
            -- (rm, evl) <- atomically $
                -- (,)
                    -- <$> readTVar (recordTraceCurrentRequestMapTVar rt)
                    -- <*> readTVar (recordTraceEventLogTVar rt)
            -- BS.Char8.putStrLn $ renderYaml (fmap Map.toList rm, evl)
            evl <- atomically $ readTVar (recordTraceEventLogTVar rt)
            BS.Char8.putStrLn $ renderYaml evl
    -- baseRecordTraceEvent rt event =
        -- atomically $ do
            -- modifyTVar' (recordTraceEventLogTVar rt) (:|> event)
            -- pure ()



newtype RecordT m (a :: *) = RecordT
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



instance TraceDomain MonadKeyValue where
    data TraceDomainRequest MonadKeyValue
        = MonadKeyValueRequestGetKV Text
        | MonadKeyValueRequestTraverseKV
        deriving (Eq, Ord, Generic)
    data TraceDomainError MonadKeyValue
        = MonadKeyValueError Void
        deriving (Eq, Ord, Generic, Aeson.ToJSON, Aeson.FromJSON)
    data TraceDomainEffect MonadKeyValue
        = MonadKeyValueEffectSetKV Text Text
        | MonadKeyValueEffectTraverseKV (Map.Map Text Text)
        deriving (Eq, Ord, Generic)
    data TraceDomainSyncPoint MonadKeyValue
        = MonadKeyValueSyncPointSetKV
        | MonadKeyValueSyncPointTraverseKV
        deriving (Eq, Ord, Generic)
    traceDomainErrorFromException _ = Nothing
    traceDomainErrorToException (MonadKeyValueError v) = absurd v

instance Aeson.ToJSON (TraceDomainRequest MonadKeyValue) where
    toJSON = Aeson.genericToJSON
        (aesonOptions (Text.length "MonadKeyValueRequest"))
    toEncoding = Aeson.genericToEncoding
        (aesonOptions (Text.length "MonadKeyValueRequest"))

instance Aeson.FromJSON (TraceDomainRequest MonadKeyValue) where
    parseJSON = Aeson.genericParseJSON
        (aesonOptions (Text.length "MonadKeyValueRequest"))

instance Aeson.ToJSON (TraceDomainEffect MonadKeyValue) where
    toJSON = Aeson.genericToJSON
        (aesonOptions (Text.length "MonadKeyValueEffect"))
    toEncoding = Aeson.genericToEncoding
        (aesonOptions (Text.length "MonadKeyValueEffect"))

instance Aeson.FromJSON (TraceDomainEffect MonadKeyValue) where
    parseJSON = Aeson.genericParseJSON
        (aesonOptions (Text.length "MonadKeyValueEffect"))

instance Aeson.ToJSON (TraceDomainSyncPoint MonadKeyValue) where
    toJSON = Aeson.genericToJSON
        (aesonOptions (Text.length "MonadKeyValueSyncPoint"))
    toEncoding = Aeson.genericToEncoding
        (aesonOptions (Text.length "MonadKeyValueSyncPoint"))

instance Aeson.FromJSON (TraceDomainSyncPoint MonadKeyValue) where
    parseJSON = Aeson.genericParseJSON
        (aesonOptions (Text.length "MonadKeyValueSyncPoint"))

instance (MonadRecorderBase m, MonadKeyValue m) => MonadKeyValue (RecordT m) where
    getKV k = RecordT $ \rt -> do
        finallyRecording rt
            (MonadKeyValueRequestGetKV k)
            (getKV k)
    setKV k v = RecordT $ \rt -> do
        recordTraceEventEffect rt
            (MonadKeyValueEffectSetKV k v)
        recordTraceEventSyncPoint rt
            MonadKeyValueSyncPointSetKV
        setKV k v
    traverseKVSatisfying cond b0 fn = RecordT $ \rt -> do
        raccum <- liftIO $ newIORef Seq.empty
        waccum <- liftIO $ newIORef Map.empty
        br <- finallyRecordingWith rt
            MonadKeyValueRequestTraverseKV
            (traverseKVSatisfying cond b0 $ \b1 k v1 -> do
                liftIO $ modifyIORef' raccum (:|> (k, v1))
                (b2, v2) <- runRecordT (fn b1 k v1) rt
                liftIO $ modifyIORef' waccum (Map.insert k v2)
                pure (b2, v2)
            )
            (\_ ->
                fmap Just $ liftIO $ readIORef raccum
            )
        wmap <- liftIO $ readIORef waccum
        recordTraceEventEffect rt
            (MonadKeyValueEffectTraverseKV wmap)
        recordTraceEventSyncPoint rt
            MonadKeyValueSyncPointTraverseKV
        pure br



test :: IO ()
test = do
    sstmvar <- newTMVarIO (Map.empty :: Map.Map Text Text)
    flip runReaderT sstmvar $ do
        recording $ do
            func



func :: MonadKeyValue m => m ()
func = do
    _ <- getKV "a"
    setKV "a" "b"
    setKV "c" "d"
    mx <- getKV "a"
    setKV "e" $ maybe "" id mx
    traverseKVSatisfying (const True) () $ \() k v -> do
        pure ((), k <> v)
    _ <- getKV "a"
    _ <- getKV "c"
    _ <- getKV "e"
    _ <- getKV "g"
    pure ()



type PreparedTraceRequestMap =
    Map.Map (TextTagged Aeson.Value) (Map.Map Int Aeson.Value)

type PreparedTraceEffects =
    Seq (TextTagged Aeson.Value)

type PreparedTraceSyncPoints =
    Map.Map (TextTagged Aeson.Value) IntSet.IntSet



data PreparedTrace = PreparedTrace
    { preparedTraceRequestMap :: !PreparedTraceRequestMap
    , preparedTraceEffects :: !PreparedTraceEffects
    , preparedTraceSyncPoints :: !PreparedTraceSyncPoints
    }
    deriving (Show, Eq, Ord, Generic)

instance Semigroup PreparedTrace where
    ptA <> ptB = PreparedTrace
        { preparedTraceRequestMap =
            Map.unionWith
                Map.union
                (preparedTraceRequestMap ptA)
                (preparedTraceRequestMap ptB)
        , preparedTraceEffects =
            preparedTraceEffects ptA <>
            preparedTraceEffects ptB
        , preparedTraceSyncPoints =
            Map.unionWith
                IntSet.union
                (preparedTraceSyncPoints ptA)
                (preparedTraceSyncPoints ptB)
        }

instance Monoid PreparedTrace where
    mempty = PreparedTrace
        { preparedTraceRequestMap = Map.empty
        , preparedTraceEffects = Seq.empty
        , preparedTraceSyncPoints = Map.empty
        }



prepareTrace ::
    Seq (TextTagged TraceEvent) ->
    PreparedTrace
prepareTrace eventSeq =
    evalState
        (getAp $ foldMap (Ap . onEvent) eventSeq)
        (0 :: Int)
  where
    onEvent (TextTagged domain (TraceEventRequest key value)) = do
        time <- get
        pure $ mempty
            { preparedTraceRequestMap =
                Map.singleton (TextTagged domain key) (Map.singleton time value)
            }
    onEvent (TextTagged domain (TraceEventEffect effect)) = do
        pure $ mempty
            { preparedTraceEffects =
                Seq.singleton (TextTagged domain effect)
            }
    onEvent (TextTagged domain (TraceEventSyncPoint marker)) = do
        oldTime <- get
        put $! (oldTime + 1)
        pure $ mempty
            { preparedTraceSyncPoints =
                Map.singleton (TextTagged domain marker) (IntSet.singleton oldTime)
            }



data ReplayError
    = ReplayErrorRequestNotFound Text Aeson.Value
    | ReplayErrorRequestParseFailed Text Aeson.Value Aeson.Value String
    deriving (Show)

instance Exception ReplayError


data ReplayContext = ReplayContext
    { replayContextTimeTVar :: TVar Int
    , replayContextRequestMap :: PreparedTraceRequestMap
    , replayContextEffectsTVar :: TVar PreparedTraceEffects
    , replayContextSyncPoints :: PreparedTraceSyncPoints
    }

createReplayContext ::
    PreparedTrace ->
    IO ReplayContext
createReplayContext pt =
    ReplayContext
        <$> newTVarIO 0
        <*> pure (preparedTraceRequestMap pt)
        <*> newTVarIO Seq.empty
        <*> pure (preparedTraceSyncPoints pt)



newtype Replayer a = Replayer
    { runReplayer ::
        ReplayContext -> IO (Either SomeException a)
    }
    deriving (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadMask)
        via (ReaderT ReplayContext (Catch.CatchT IO))

class (MonadMask m) => MonadReplayer m where
    baseReplayer ::
        (ReplayContext -> IO (Either SomeException a)) ->
        m a

instance MonadReplayer Replayer where
    baseReplayer = Replayer



replayerLiftIO :: (MonadReplayer m) => IO a -> m a
replayerLiftIO io = baseReplayer (\_ -> Right <$> io)



replayTraceEventRequestRaw ::
    ReplayContext ->
    Text ->
    Aeson.Value ->
    IO (Maybe Aeson.Value)
replayTraceEventRequestRaw rt domain key =
    atomically $ do
        time <- readTVar (replayContextTimeTVar rt)
        pure $ do
            responses <-
                Map.lookup (TextTagged domain key) (replayContextRequestMap rt)
            (_, resp) <-
                Map.lookupLE time responses <|> Map.lookupMin responses
            Just resp

replayTraceEventEffectRaw ::
    ReplayContext ->
    Text ->
    Aeson.Value ->
    IO ()
replayTraceEventEffectRaw rt domain effect =
    atomically $ do
        modifyTVar'
            (replayContextEffectsTVar rt)
            (:|> TextTagged  domain effect)

replayTraceEventSyncPointRaw ::
    ReplayContext ->
    Text ->
    Aeson.Value ->
    IO ()
replayTraceEventSyncPointRaw rt domain marker = do
    atomically $ do
        oldTime <- readTVar (replayContextTimeTVar rt)
        let mbNewTime = do
                timeSet <-
                    Map.lookup
                        (TextTagged domain marker)
                        (replayContextSyncPoints rt)
                IntSet.lookupGT oldTime timeSet
        case mbNewTime of
            Just newTime ->
                writeTVar (replayContextTimeTVar rt) newTime
            Nothing ->
                pure ()



replayTraceEventRequest ::
    forall r d m.
    (TraceDomain d, Aeson.FromJSON r, MonadReplayer m) =>
    TraceDomainRequest d ->
    m r
replayTraceEventRequest key =
    baseReplayer $ \rt -> do
        mbResultJson <-
            replayTraceEventRequestRaw rt (domainName $ Proxy @d) (Aeson.toJSON key)
        case mbResultJson of
            Just resultJson ->
                case
                    Aeson.fromJSON resultJson ::
                        Aeson.Result (Result (TraceDomainError d) r)
                  of
                    Aeson.Success (Success result) ->
                        pure (Right result)
                    Aeson.Success (Failure domErr) ->
                        pure (Left (traceDomainErrorToException domErr))
                    Aeson.Error errMsg ->
                        throwM $ ReplayErrorRequestParseFailed
                            (domainName $ Proxy @d)
                            (Aeson.toJSON key)
                            resultJson
                            errMsg
            Nothing ->
                throwM $ ReplayErrorRequestNotFound
                    (domainName $ Proxy @d)
                    (Aeson.toJSON key)

replayTraceEventEffect ::
    forall d m.
    (TraceDomain d, MonadReplayer m) =>
    TraceDomainEffect d ->
    m ()
replayTraceEventEffect effect =
    baseReplayer $ \rt -> do
        replayTraceEventEffectRaw rt (domainName $ Proxy @d) (Aeson.toJSON effect)
        pure (Right ())

replayTraceEventSyncPoint ::
    forall d m.
    (TraceDomain d, MonadReplayer m) =>
    TraceDomainSyncPoint d ->
    m ()
replayTraceEventSyncPoint marker =
    baseReplayer $ \rt -> do
        replayTraceEventSyncPointRaw rt (domainName $ Proxy @d) (Aeson.toJSON marker)
        pure (Right ())



instance MonadKeyValue Replayer where
    getKV k =
        replayTraceEventRequest
            (MonadKeyValueRequestGetKV k)
    setKV k v = do
        replayTraceEventEffect
            (MonadKeyValueEffectSetKV k v)
        replayTraceEventSyncPoint
            MonadKeyValueSyncPointSetKV
    traverseKVSatisfying _ b0 fn = do
        vs <- replayTraceEventRequest
            MonadKeyValueRequestTraverseKV
        waccum <- replayerLiftIO $ newIORef Map.empty
        br <- foldlM
            (\b1 (k, v1) -> do
                (b2, v2) <- fn b1 k v1
                replayerLiftIO $ modifyIORef' waccum (Map.insert k v2)
                pure b2
            )
            b0
            (vs :: Seq (Text, Text))
        wmap <- replayerLiftIO $ readIORef waccum
        replayTraceEventEffect
            (MonadKeyValueEffectTraverseKV wmap)
        replayTraceEventSyncPoint
            MonadKeyValueSyncPointTraverseKV
        pure br



unJSON :: (Aeson.FromJSON a) => Aeson.Value -> a
unJSON j = case Aeson.fromJSON j of
    Aeson.Success x -> x
    Aeson.Error e -> error e

testTrace :: Seq (TextTagged TraceEvent)
testTrace = unJSON $ [Yaml.TH.yamlQQ|
  - MonadKeyValue:
      request:
      - getKV: a
      - success: null
  - MonadKeyValue:
      effect:
        setKV:
        - a
        - b
  - MonadKeyValue:
      syncPoint: setKV
  - MonadKeyValue:
      effect:
        setKV:
        - c
        - d
  - MonadKeyValue:
      syncPoint: setKV
  - MonadKeyValue:
      request:
      - getKV: a
      - success: b
  - MonadKeyValue:
      effect:
        setKV:
        - e
        - b
  - MonadKeyValue:
      syncPoint: setKV
  - MonadKeyValue:
      request:
      - traverseKV: []
      - success:
        - - a
          - b
        - - c
          - d
        - - e
          - b
  - MonadKeyValue:
      effect:
        traverseKV:
          a: ab
          c: cd
          e: eb
  - MonadKeyValue:
      syncPoint: traverseKV
  - MonadKeyValue:
      request:
      - getKV: a
      - success: ab
  - MonadKeyValue:
      request:
      - getKV: c
      - success: cd
  - MonadKeyValue:
      request:
      - getKV: e
      - success: eb
  - MonadKeyValue:
      request:
      - getKV: g
      - success: null
  |]

preparedTestTrace :: PreparedTrace
preparedTestTrace = prepareTrace testTrace



testReplay :: IO ()
testReplay = do
    rt <- createReplayContext preparedTestTrace
    er <- flip runReplayer rt $ do
        func
    effects <- atomically $ readTVar (replayContextEffectsTVar rt)
    BS.Char8.putStrLn $ renderYaml effects
    print er
