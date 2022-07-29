{-# LANGUAGE BangPatterns #-}
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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Client.Traced
    -- (
      -- Reducible (..)
    -- , applyLifting
    -- , Lifting (..)
    -- , MonadLifting (..)
    -- )
where

import Control.Applicative
import Control.Concurrent.STM
import Control.DeepSeq (NFData)
import Control.Exception (ErrorCall (..))
import Control.Monad.Catch
import Control.Monad.Except
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
import Data.Word
import GHC.Generics
import qualified Type.Reflection as Type
import qualified Control.Concurrent.Async as Async
import qualified Control.Monad.Catch.Pure as Catch
import qualified Control.Monad.Codensity as Codensity
import qualified Control.Record as Record
import qualified Control.Reducible as Reducible
import qualified Data.Aeson as Aeson
import qualified Data.Binary.Builder as Binary.Builder
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Data.CaseInsensitive as CI
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Streaming.Zlib as Zlib
import qualified Data.Text as Text
import qualified Data.Yaml.Pretty as Yaml.Pretty
import qualified Data.Yaml.TH as Yaml.TH
import qualified Network.HTTP.Client
import qualified Network.HTTP.Client.Internal
import qualified Network.HTTP.Media
import qualified Network.HTTP.Types
import qualified Servant.Client
import qualified Servant.Client.Core
import qualified Servant.Client.Core.RunClient
import qualified Servant.Client.Internal.HttpClient.Streaming
import qualified Servant.Client.Streaming
import qualified Servant.Client.Traced.SerializedByteString as SBS
import qualified Servant.Client.Traced.RandomId as RandomId
import qualified Servant.Types.SourceT
import qualified System.IO.Unsafe

type Client = ClientFC 'NoStreaming

type StreamingClient = ClientFC 'Streaming

client ::
    forall api.
    (Servant.Client.Streaming.HasClient Client api) =>
    Proxy api ->
    Servant.Client.Streaming.Client Client api
client p =
    Servant.Client.Core.clientIn
        p
        (Proxy @Client)

streamingClient ::
    forall api.
    (Servant.Client.Streaming.HasClient StreamingClient api) =>
    Proxy api ->
    Servant.Client.Streaming.Client StreamingClient api
streamingClient p =
    Servant.Client.Core.clientIn
        p
        (Proxy @StreamingClient)

data IsStreaming = Streaming | NoStreaming

-- newtype TracedClientM (s :: IsStreaming) a = TracedClientM
    -- { runTracedClientM ::
        -- Record.RecordTrace ->
        -- Servant.Client.Streaming.ClientM a
    -- }
    -- deriving (Functor, Applicative, Monad)
        -- via (ReaderT Record.RecordTrace Servant.Client.Streaming.ClientM)

type ClientFCReqHandler q =
    Maybe [Network.HTTP.Types.Status] ->
    Servant.Client.Core.Request ->
    (Servant.Client.Streaming.ClientError -> q) ->
    (Servant.Client.Core.Response -> q) ->
    q

type ClientFCWithSReqHandler q =
    forall t.
    Servant.Client.Core.Request ->
    (Servant.Client.Streaming.StreamingResponse -> IO t) ->
    (Servant.Client.Streaming.ClientError -> q) ->
    (t -> q) ->
    q

newtype ClientFC (s :: IsStreaming) a = ClientFC
    { runClientFC ::
        forall q.
        (Servant.Client.Streaming.ClientError -> q) ->
        ClientFCReqHandler q ->
        ClientFCWithSReqHandler q ->
        (IO q ->q) ->
        (a -> q) ->
        q
    }

instance Functor (ClientFC s) where
    fmap f ma =
        ClientFC $ \onThrow onReq onWithSReq onIO cont ->
            runClientFC ma onThrow onReq onWithSReq onIO $ \a ->
                cont (f a)

instance Applicative (ClientFC s) where
    pure x =
        ClientFC $ \_ _ _ _ cont ->
            cont x
    mf <*> mb =
        ClientFC $ \onThrow onReq onWithSReq onIO cont ->
            runClientFC mf onThrow onReq onWithSReq onIO $ \f ->
                runClientFC mb onThrow onReq onWithSReq onIO $ \b ->
                    cont (f b)

instance Monad (ClientFC s) where
    ma >>= sel =
        ClientFC $ \onThrow onReq onWithSReq onIO cont ->
            runClientFC ma onThrow onReq onWithSReq onIO $ \a ->
                runClientFC (sel a) onThrow onReq onWithSReq onIO cont

instance MonadError Servant.Client.Streaming.ClientError (ClientFC s) where
    throwError clientError =
        ClientFC $ \onThrow _onReq _onWithSReq _onIO _cont ->
            onThrow clientError
    catchError act handler =
        ClientFC $ \onThrow onReq onWithSReq onIO cont ->
            let handlingThrow ex =
                    runClientFC (handler ex) onThrow onReq onWithSReq onIO cont
            in
            runClientFC act handlingThrow onReq onWithSReq onIO cont

instance Servant.Client.Core.RunClient (ClientFC s) where
    runRequestAcceptStatus mbStatusList coreRequest =
        ClientFC $ \onThrow onReq _onWithSReq _onIO cont ->
            onReq mbStatusList coreRequest onThrow cont
    throwClientError clientError =
        ClientFC $ \onThrow _onReq _onWithSReq _onIO _cont ->
            onThrow clientError

instance Servant.Client.Core.RunStreamingClient (ClientFC s) where
    withStreamingRequest coreRequest inner =
        ClientFC $ \onThrow _onReq onWithSReq _onIO cont ->
            onWithSReq coreRequest inner onThrow cont



class (Monad m) => MonadServantClient m where
    runClientM ::
        ClientFC 'NoStreaming a ->
        m (Either Servant.Client.ClientError a)

instance
    {-# OVERLAPPABLE #-}
    ( Reducible.Reducible m
    , Monad m
    , MonadServantClient (Reducible.Reduced m)
    ) =>
    MonadServantClient m
  where
    runClientM cfc =
        Reducible.fromReduced $
            runClientM cfc

instance
    ( Reducible.MonadLifting m n
    , Monad m
    , MonadServantClient n
    ) =>
    MonadServantClient (Reducible.Lifting m n)
  where
    runClientM cfc =
        Reducible.applyLifting $
            runClientM cfc



class (MonadServantClient m) => MonadServantClientStreaming m where
    withClientM ::
        ClientFC s a ->
        (Either Servant.Client.ClientError a -> m b) ->
        m b

instance
    (MonadServantClientStreaming m) =>
    MonadServantClientStreaming (ExceptT r m)
  where
    withClientM cfc inner =
        ExceptT $
            withClientM cfc (\x -> runExceptT (inner x))

instance
    (MonadServantClientStreaming m) =>
    MonadServantClientStreaming (ReaderT r m)
  where
    withClientM cfc inner =
        ReaderT $ \ctx ->
            withClientM cfc (\x -> runReaderT (inner x) ctx)

instance
    (MonadServantClientStreaming m) =>
    MonadServantClientStreaming (StateT r m)
  where
    withClientM cfc inner =
        StateT $ \s ->
            withClientM cfc (\x -> runStateT (inner x) s)

instance
    {-# OVERLAPPABLE #-}
    ( Reducible.Reducible m
    , Monad m
    , MonadServantClientStreaming (Reducible.Reduced m)
    ) =>
    MonadServantClientStreaming m
  where
    withClientM cfc inner =
        Reducible.fromReduced $
            withClientM cfc (Reducible.toReduced . inner)



instance Record.TraceDomain MonadServantClient where
    data TraceDomainRequest MonadServantClient
        = ServantClientRequestStart SerializedRequest
        | ServantClientRequestOutcome Text
        | ServantClientRequestResponseChunk Text Int
        deriving (Show, Eq, Ord, Generic)
    newtype TraceDomainError MonadServantClient
        = ServantClientError Void
        deriving (Show, Eq, Ord, Generic)
    data TraceDomainEffect MonadServantClient
        = ServantClientEffectSetAcceptStatus Text [(Int, SBS.SerializedByteString)]
        | ServantClientEffectRequest Text SerializedRequest
        | ServantClientEffectRequestChunk Text SBS.SerializedByteString
        deriving (Show, Eq, Ord, Generic)
    data TraceDomainSyncPoint MonadServantClient
        = ServantClientSyncPointRequest SerializedRequest
        deriving (Show, Eq, Ord, Generic)
    traceDomainErrorFromException _ = Nothing
    traceDomainErrorToException (ServantClientError v) = absurd v

instance Aeson.ToJSON (Record.TraceDomainRequest MonadServantClient) where
    toJSON = Aeson.genericToJSON
        (Record.aesonOptions (Text.length "ServantClientRequest"))
    toEncoding = Aeson.genericToEncoding
        (Record.aesonOptions (Text.length "ServantClientRequest"))

instance Aeson.FromJSON (Record.TraceDomainRequest MonadServantClient) where
    parseJSON = Aeson.genericParseJSON
        (Record.aesonOptions (Text.length "ServantClientRequest"))

instance Aeson.ToJSON (Record.TraceDomainError MonadServantClient)

instance Aeson.FromJSON (Record.TraceDomainError MonadServantClient)

instance Aeson.ToJSON (Record.TraceDomainEffect MonadServantClient) where
    toJSON = Aeson.genericToJSON
        (Record.aesonOptions (Text.length "ServantClientEffect"))
    toEncoding = Aeson.genericToEncoding
        (Record.aesonOptions (Text.length "ServantClientEffect"))

instance Aeson.FromJSON (Record.TraceDomainEffect MonadServantClient) where
    parseJSON = Aeson.genericParseJSON
        (Record.aesonOptions (Text.length "ServantClientEffect"))

instance Aeson.ToJSON (Record.TraceDomainSyncPoint MonadServantClient) where
    toJSON = Aeson.genericToJSON
        (Record.aesonOptions (Text.length "ServantClientSyncPoint"))
    toEncoding = Aeson.genericToEncoding
        (Record.aesonOptions (Text.length "ServantClientSyncPoint"))

instance Aeson.FromJSON (Record.TraceDomainSyncPoint MonadServantClient) where
    parseJSON = Aeson.genericParseJSON
        (Record.aesonOptions (Text.length "ServantClientSyncPoint"))



instance
    {-# OVERLAPPING #-}
    MonadServantClient (ReaderT Servant.Client.ClientEnv IO)
  where
    runClientM cfc = withClientM cfc pure

-- TODO: add a function like "liftBracket" into reducible

instance
    {-# OVERLAPPING #-}
    MonadServantClientStreaming (ReaderT Servant.Client.ClientEnv IO)
  where
    withClientM cfc inner = ReaderT $ \cenv ->
        Servant.Client.Streaming.withClientM
            (runClientFCStreaming cfc)
            cenv
            (\result -> runReaderT (inner result) cenv)

runClientFCStreaming ::
    ClientFC s a ->
    Servant.Client.Streaming.ClientM a
runClientFCStreaming cfc = do
    runClientFC cfc
        (\err ->
            Servant.Client.Core.throwClientError err
        )
        (\mbStatusList coreRequest innerCatch innerCont ->
            wrapClientM $ \env outerCont ->
                unwrappedRunRequestAcceptStatus
                    mbStatusList
                    coreRequest
                    env
                    (\eiResponse ->
                        unwrapClientM
                            (either innerCatch innerCont eiResponse)
                            env
                            outerCont
                    )
        )
        (\coreRequest act innerCatch innerCont ->
            wrapClientM $ \env outerCont ->
                fixedUnwrappedWithStreamingRequest
                    coreRequest
                    act
                    env
                    (\eiResult -> do
                        unwrapClientM
                            (either innerCatch innerCont eiResult)
                            env
                            outerCont
                    )
        )
        (\io ->
            join $ liftIO io
        )
        pure



instance
    ( Record.MonadRecorderBase m
    , MonadServantClient m
    ) =>
    MonadServantClient (Record.RecordT m)
  where
    runClientM cfc = Record.RecordT $ \rt -> do
        runClientM
            (wrapRecordingClientFC rt cfc)

instance
    ( Record.MonadRecorderBase m
    , MonadServantClientStreaming m
    ) =>
    MonadServantClientStreaming (Record.RecordT m)
  where
    withClientM cfc inner = Record.RecordT $ \rt -> do
        withClientM
            (wrapRecordingClientFC rt cfc)
            (\result -> Record.runRecordT (inner result) rt)

wrapRecordingClientFC ::
    Record.RecordTrace ->
    ClientFC s a ->
    ClientFC s a
wrapRecordingClientFC rt cfc =
    ClientFC $ \onThrow onReq onWithSReq onIO outerCont ->
        runClientFC cfc
            onThrow
            (wrapRecordingOnReq rt onIO onReq)
            (wrapRecordingOnWithSReq rt onIO onWithSReq)
            onIO
            outerCont

wrapRecordingOnReq ::
    Record.RecordTrace ->
    (IO q -> q) ->
    ClientFCReqHandler q ->
    ClientFCReqHandler q
wrapRecordingOnReq rt onIO onReq =
    \mbStatusList coreRequest innerCatch innerCont ->
        onIO $ do
            reqId <- recordRequestStart rt mbStatusList coreRequest
            pure $
                onReq
                    mbStatusList
                    (wrapRecordingRequest rt reqId coreRequest)
                    (\clientError ->
                        onIO $ do
                            recordClientError rt reqId clientError
                            pure $ innerCatch clientError
                    )
                    (\response ->
                        onIO $ do
                            recordSimpleResponse rt reqId response
                            pure $ innerCont response
                    )

wrapRecordingOnWithSReq ::
    Record.RecordTrace ->
    (IO q -> q) ->
    ClientFCWithSReqHandler q ->
    ClientFCWithSReqHandler q
wrapRecordingOnWithSReq rt onIO onWithSReq =
    \coreRequest act innerCatch innerCont ->
        onIO $ do
            reqId <- recordRequestStart rt Nothing coreRequest
            pure $
                onWithSReq
                    (wrapRecordingRequest rt reqId coreRequest)
                    (\streamingResponse -> do
                        updatedResponse <-
                            recordStreamingResponse rt reqId streamingResponse
                        act updatedResponse
                    )
                    (\clientError ->
                        onIO $ do
                            recordClientError rt reqId clientError
                            pure $ innerCatch clientError
                    )
                    innerCont

recordRequestStart ::
    Record.RecordTrace ->
    Maybe [Network.HTTP.Types.Status] ->
    Servant.Client.Core.Request ->
    IO Text
recordRequestStart rt mbStatusList coreRequest = do
    reqId <- RandomId.generateRandomId
    let serializedRequest = serializeRequest coreRequest
    Record.recordTraceEventRequestResult rt
        (ServantClientRequestStart serializedRequest)
        reqId
    Record.recordTraceEventSyncPoint rt
        (ServantClientSyncPointRequest serializedRequest)
    Record.recordTraceEventEffect rt
        (ServantClientEffectRequest reqId serializedRequest)
    forM_ mbStatusList $ \statusList -> do
        Record.recordTraceEventEffect rt
            (ServantClientEffectSetAcceptStatus
                reqId
                (fmap serializeStatusCode statusList)
            )
    pure reqId

recordClientError ::
    Record.RecordTrace ->
    Text ->
    Servant.Client.Streaming.ClientError ->
    IO ()
recordClientError rt reqId clientError = do
    Record.recordTraceEventRequestResult rt
        (ServantClientRequestOutcome reqId)
        (RequestOutcomeError $
            serializeClientError clientError
        )

recordSimpleResponse ::
    Record.RecordTrace ->
    Text ->
    Servant.Client.Response ->
    IO ()
recordSimpleResponse rt reqId response = do
    let serializedResponse =
            serializeResponse response
    Record.recordTraceEventRequestResult rt
        (ServantClientRequestOutcome reqId)
        (RequestOutcomeResponse serializedResponse)

recordStreamingResponse ::
    Record.RecordTrace ->
    Text ->
    Servant.Client.Streaming.StreamingResponse ->
    IO Servant.Client.Streaming.StreamingResponse
recordStreamingResponse rt reqId streamingResponse = do
    let (serializedResponse, updatedResponse) =
            makeSerializedStreamingResponse
                rt reqId streamingResponse
    Record.recordTraceEventRequestResult rt
        (ServantClientRequestOutcome reqId)
        (RequestOutcomeResponse serializedResponse)
    pure updatedResponse



fixedUnwrappedWithStreamingRequest ::
    Servant.Client.Core.Request ->
    (Servant.Client.Streaming.StreamingResponse -> IO a) ->
    Servant.Client.Streaming.ClientEnv ->
    (Either Servant.Client.Streaming.ClientError a -> IO r) ->
    IO r
fixedUnwrappedWithStreamingRequest req inner env cont = do
    {- withStreamingRequest throws errors in IO -}
    mask $ \restore -> do
        handlingRequestIORef <- newIORef True
        eiResult <- try $ restore $
            unwrappedWithStreamingRequest
                req
                (\resp -> do
                    writeIORef handlingRequestIORef False
                    inner resp
                )
                env
                cont
        case eiResult of
            Right result -> pure result
            Left ex -> do
                let clientError =
                        fromMaybe
                            (Servant.Client.ConnectionError ex)
                            (fromException ex)
                handlingRequest <- readIORef handlingRequestIORef
                if handlingRequest
                    then restore $ cont (Left clientError)
                    else throwM clientError

unwrappedRunRequestAcceptStatus ::
    Maybe [Network.HTTP.Types.Status] ->
    Servant.Client.Core.Request ->
    Servant.Client.Streaming.ClientEnv ->
    (Either Servant.Client.Streaming.ClientError Servant.Client.Streaming.Response -> IO r) ->
    IO r
unwrappedRunRequestAcceptStatus mbStatus req env cont =
    unwrapClientM
        (Servant.Client.Core.RunClient.runRequestAcceptStatus mbStatus req)
        env
        cont

unwrappedWithStreamingRequest ::
    Servant.Client.Core.Request ->
    (Servant.Client.Streaming.StreamingResponse -> IO a) ->
    Servant.Client.Streaming.ClientEnv ->
    (Either Servant.Client.Streaming.ClientError a -> IO r) ->
    IO r
unwrappedWithStreamingRequest req inner env cont =
    unwrapClientM
        (Servant.Client.Core.RunClient.withStreamingRequest req $ \resp ->
            inner resp
        )
        env
        cont

unwrapClientM ::
    Servant.Client.Streaming.ClientM a ->
    Servant.Client.Streaming.ClientEnv ->
    (Either Servant.Client.Streaming.ClientError a -> IO r) ->
    IO r
unwrapClientM m env =
    Codensity.runCodensity $
    runExceptT $
    flip runReaderT env $
    Servant.Client.Internal.HttpClient.Streaming.unClientM m

wrapClientM ::
    (forall r.
        Servant.Client.Streaming.ClientEnv ->
        (Either Servant.Client.Streaming.ClientError a -> IO r) ->
        IO r
    ) ->
    Servant.Client.Streaming.ClientM a
wrapClientM fn =
    Servant.Client.Internal.HttpClient.Streaming.ClientM $
    ReaderT $ \env ->
    ExceptT $
    Codensity.Codensity $ \k ->
    fn env k



instance MonadServantClient Record.Replayer where
    runClientM cfc = withClientM cfc pure

instance MonadServantClientStreaming Record.Replayer where
    withClientM = withClientFCReplay

withClientFCReplay ::
    ClientFC s a ->
    (Either Servant.Client.ClientError a -> Record.Replayer r) ->
    Record.Replayer r
withClientFCReplay cfc outerCont = do
    runClientFC cfc
        (outerCont . Left)
        (\mbStatusList coreRequest innerCatch innerCont -> do
            replayRequest mbStatusList coreRequest
                (innerCatch . restoreClientError)
                (\streamingResponse -> do
                    response <- consolidateResponse streamingResponse
                    innerCont response
                )
        )
        (\coreRequest act innerCatch innerCont -> do
            replayRequest Nothing coreRequest
                (innerCatch . restoreClientError)
                (\streamingResponse -> do
                    r <- Record.replayerLiftIO $ act streamingResponse
                    innerCont r
                )
        )
        (\io ->
            join $ Record.replayerLiftIO io
        )
        (outerCont . Right)

replayRequest ::
    Maybe [Network.HTTP.Types.Status] ->
    Servant.Client.Core.Request ->
    (SerializedClientError -> Record.Replayer r) ->
    (Servant.Client.Streaming.StreamingResponse -> Record.Replayer r) ->
    Record.Replayer r
replayRequest mbStatusList coreRequest onClientError onResponse = do
    let serializedRequest = serializeRequest coreRequest
    reqId <-
        Record.replayTraceEventRequest
            (ServantClientRequestStart serializedRequest)
    Record.replayTraceEventSyncPoint
        (ServantClientSyncPointRequest serializedRequest)
    Record.replayTraceEventEffect
        (ServantClientEffectRequest reqId serializedRequest)
    forM_ mbStatusList $ \statusList -> do
        Record.replayTraceEventEffect
            (ServantClientEffectSetAcceptStatus
                reqId
                (fmap serializeStatusCode statusList)
            )
    replayDrainRequest reqId coreRequest
    requestOutcome <-
        Record.replayTraceEventRequest
            (ServantClientRequestOutcome reqId)
    case requestOutcome of
        RequestOutcomeError clientError ->
            onClientError clientError
        RequestOutcomeResponse responseHeader -> do
            streamingResponse <-
                replayRestoreStreamingResponse reqId responseHeader
            onResponse streamingResponse



data SerializedRequest = SerializedRequest
    { requestHttpVersion ::
        (Int, Int)
    , requestMethod ::
        SBS.SerializedByteString
    , requestPath ::
        SBS.SerializedByteString
    , requestQueryString ::
        Seq (SBS.SerializedByteString, Maybe SBS.SerializedByteString)
    , requestAccept ::
        Seq SBS.SerializedByteString
    , requestHeaders ::
        Seq (SBS.SerializedByteString, SBS.SerializedByteString)
    , requestBody ::
        Maybe (SBS.SerializedByteString, Maybe SBS.SerializedByteString)
    }
    deriving (Show, Eq, Ord, Generic)

instance Aeson.ToJSON SerializedRequest

instance Aeson.FromJSON SerializedRequest



serializeStrippedRequest ::
    Servant.Client.Core.RequestF () (Servant.Client.BaseUrl, BS.ByteString) ->
    (Text, SerializedRequest)
serializeStrippedRequest coreRequest =
    (baseUrl, serialized)
  where
    baseUrl =
        Text.pack $
        Servant.Client.showBaseUrl $
        fst $
        Servant.Client.Core.requestPath coreRequest
    mbSerialBody =
        case Servant.Client.Core.requestBody coreRequest of
            Nothing -> Nothing
            Just ((), coreMediaType) ->
                ( Just (serialMediaType, Just "<stripped>")
                )
              where
                serialMediaType =
                    SBS.SerializedByteString $
                    Network.HTTP.Media.renderHeader coreMediaType
    serialized =
        SerializedRequest
            { requestHttpVersion =
                case Servant.Client.Core.requestHttpVersion coreRequest of
                    Network.HTTP.Types.HttpVersion v1 v2 -> (v1, v2)
            , requestMethod =
                SBS.SerializedByteString $
                Servant.Client.Core.requestMethod coreRequest
            , requestPath =
                SBS.SerializedByteString $
                snd $
                Servant.Client.Core.requestPath coreRequest
            , requestQueryString =
                fmap
                    (\(name, mbValue) ->
                        ( SBS.SerializedByteString name
                        , fmap SBS.SerializedByteString mbValue
                        )
                    ) $
                Servant.Client.Core.requestQueryString coreRequest
            , requestAccept =
                fmap
                    (   SBS.SerializedByteString .
                        Network.HTTP.Media.renderHeader
                    ) $
                Servant.Client.Core.requestAccept coreRequest
            , requestHeaders =
                fmap
                    (\(name, value) ->
                        ( SBS.SerializedByteString $ CI.foldedCase name
                        , SBS.SerializedByteString value
                        )
                    ) $
                Servant.Client.Core.requestHeaders coreRequest
            , requestBody =
                mbSerialBody
            }

restoreStrippedRequest ::
    Text ->
    SerializedRequest ->
    Servant.Client.Core.RequestF () (Servant.Client.BaseUrl, BS.ByteString)
restoreStrippedRequest baseUrlText serialRequest =
    Servant.Client.Core.Request
        { Servant.Client.Core.requestPath =
            ( baseUrl
            , SBS.getSerializedByteString $
                requestPath serialRequest
            )
        , Servant.Client.Core.requestQueryString =
                fmap
                    (\(name, mbValue) ->
                        ( SBS.getSerializedByteString name
                        , fmap SBS.getSerializedByteString mbValue
                        )
                    ) $
                requestQueryString serialRequest
        , Servant.Client.Core.requestBody =
            mbCoreBody
        , Servant.Client.Core.requestAccept =
            accept
        , Servant.Client.Core.requestHeaders =
            fmap
                (\(name, value) ->
                    ( CI.mk $ SBS.getSerializedByteString name
                    , SBS.getSerializedByteString value
                    )
                ) $
            requestHeaders serialRequest
        , Servant.Client.Core.requestHttpVersion =
            case requestHttpVersion serialRequest of
                (v1, v2) -> Network.HTTP.Types.HttpVersion v1 v2
        , Servant.Client.Core.requestMethod =
            SBS.getSerializedByteString $
            requestMethod serialRequest
        }
  where
    !baseUrl =
        System.IO.Unsafe.unsafeDupablePerformIO $
        Servant.Client.parseBaseUrl $
        Text.unpack baseUrlText
    !mbCoreBody =
        case requestBody serialRequest of
            Nothing -> Nothing
            Just (serialMediaType, _) -> Just ((), coreMT)
              where
                !coreMT = parseMediaType serialMediaType
    !accept =
        foldl'
            (\r x -> r Seq.:|> parseMediaType x)
            Seq.Empty $
        requestAccept serialRequest
    parseMediaType =
        maybe (error "bad accept") id .
        Network.HTTP.Media.parseAccept .
        SBS.getSerializedByteString

serializeRequest ::
    Servant.Client.Core.Request ->
    SerializedRequest
serializeRequest coreRequest =
    SerializedRequest
        { requestHttpVersion =
            case Servant.Client.Core.requestHttpVersion coreRequest of
                Network.HTTP.Types.HttpVersion v1 v2 -> (v1, v2)
        , requestMethod =
            SBS.SerializedByteString $
            Servant.Client.Core.requestMethod coreRequest
        , requestPath =
            SBS.SerializedByteString $
            BS.Lazy.toStrict $
            Binary.Builder.toLazyByteString $
            Servant.Client.Core.requestPath coreRequest
        , requestQueryString =
            fmap
                (\(name, mbValue) ->
                    ( SBS.SerializedByteString name
                    , fmap SBS.SerializedByteString mbValue
                    )
                ) $
            Servant.Client.Core.requestQueryString coreRequest
        , requestAccept =
            fmap
                (   SBS.SerializedByteString .
                    Network.HTTP.Media.renderHeader
                ) $
            Servant.Client.Core.requestAccept coreRequest
        , requestHeaders =
            fmap
                (\(name, value) ->
                    ( SBS.SerializedByteString $ CI.foldedCase name
                    , SBS.SerializedByteString value
                    )
                ) $
            Servant.Client.Core.requestHeaders coreRequest
        , requestBody =
            fmap
                (\(coreContents, coreMediaType) ->
                    ( SBS.SerializedByteString $
                        Network.HTTP.Media.renderHeader coreMediaType
                    , serializeRequestBody coreContents
                    )
                ) $
            Servant.Client.Core.requestBody coreRequest
        }

replayDrainRequest ::
    Text ->
    Servant.Client.Core.Request ->
    Record.Replayer ()
replayDrainRequest reqId coreRequest =
    case Servant.Client.Core.requestBody coreRequest of
        Nothing -> pure ()
        Just (coreBody, _) ->
            replayDrainRequestBody reqId coreBody

wrapRecordingRequest ::
    Record.RecordTrace ->
    Text ->
    Servant.Client.Core.Request ->
    Servant.Client.Core.Request
wrapRecordingRequest rt reqId coreRequest =
    coreRequest
        { Servant.Client.Core.requestBody =
            fmap (first (wrapRecordingRequestBody rt reqId)) $
            Servant.Client.Core.requestBody coreRequest
        }
  where
    first f (a, b) = (f a, b)

serializeRequestBody ::
    Servant.Client.Core.RequestBody ->
    Maybe SBS.SerializedByteString
serializeRequestBody coreBody =
    case coreBody of
        Servant.Client.Core.RequestBodyLBS lbs ->
            Just $ SBS.SerializedByteString $ BS.Lazy.toStrict lbs
        Servant.Client.Core.RequestBodyBS bs ->
            Just $ SBS.SerializedByteString bs
        Servant.Client.Core.RequestBodySource _ ->
            Nothing

wrapRecordingRequestBody ::
    Record.RecordTrace ->
    Text ->
    Servant.Client.Core.RequestBody ->
    Servant.Client.Core.RequestBody
wrapRecordingRequestBody rt reqId coreBody =
    case coreBody of
        Servant.Client.Core.RequestBodyLBS _ ->
            coreBody
        Servant.Client.Core.RequestBodyBS _ ->
            coreBody
        Servant.Client.Core.RequestBodySource sio ->
            Servant.Client.Core.RequestBodySource $
                wrapRecordingSourceT rt reqId sio

replayDrainRequestBody ::
    Text ->
    Servant.Client.Core.RequestBody ->
    Record.Replayer ()
replayDrainRequestBody reqId coreBody =
    case coreBody of
        Servant.Client.Core.RequestBodyLBS _ ->
            pure ()
        Servant.Client.Core.RequestBodyBS _ ->
            pure ()
        Servant.Client.Core.RequestBodySource sio ->
            replayDrainSourceT reqId sio

wrapRecordingSourceT ::
    Record.RecordTrace ->
    Text ->
    Servant.Types.SourceT.SourceT IO BS.Lazy.ByteString ->
    Servant.Types.SourceT.SourceT IO BS.Lazy.ByteString
wrapRecordingSourceT rt reqId = Servant.Types.SourceT.mapStepT go
  where
    go Servant.Types.SourceT.Stop =
        Servant.Types.SourceT.Stop
    go (Servant.Types.SourceT.Error msg) =
        Servant.Types.SourceT.Error msg
    go (Servant.Types.SourceT.Skip next) =
        Servant.Types.SourceT.Skip (go next)
    go (Servant.Types.SourceT.Yield lbs next) =
        Servant.Types.SourceT.Effect $ do
            Record.recordTraceEventEffect rt
                (ServantClientEffectRequestChunk
                    reqId
                    (SBS.SerializedByteString $ BS.Lazy.toStrict lbs)
                )
            pure $ Servant.Types.SourceT.Yield lbs (go next)
    go (Servant.Types.SourceT.Effect m) =
        Servant.Types.SourceT.Effect (go <$> m)

replayDrainSourceT ::
    Text ->
    Servant.Types.SourceT.SourceT IO BS.Lazy.ByteString ->
    Record.Replayer ()
replayDrainSourceT reqId sourcet =
    Record.Replayer $ \rt ->
        Servant.Types.SourceT.unSourceT
            sourcet
            (\stept -> Record.runReplayer (go stept) rt)
  where
    go Servant.Types.SourceT.Stop =
        pure ()
    go (Servant.Types.SourceT.Error msg) =
        throwM $ ErrorCall msg
    go (Servant.Types.SourceT.Skip next) =
        go next
    go (Servant.Types.SourceT.Yield lbs next) = do
        Record.replayTraceEventEffect
            (ServantClientEffectRequestChunk
                reqId
                (SBS.SerializedByteString $ BS.Lazy.toStrict lbs)
            )
        go next
    go (Servant.Types.SourceT.Effect m) =
        Record.replayerLiftIO m >>= go



data RequestOutcome
    = RequestOutcomeError SerializedClientError
    | RequestOutcomeResponse SerializedResponse
    deriving (Show, Eq, Ord, Generic)

instance Aeson.ToJSON RequestOutcome

instance Aeson.FromJSON RequestOutcome

data SerializedResponse = SerializedResponse
    { responseStatusCode ::
        (Int, SBS.SerializedByteString)
    , responseHeaders ::
        Seq (SBS.SerializedByteString, SBS.SerializedByteString)
    , responseHttpVersion ::
        (Int, Int)
    , responseBody ::
        Maybe SBS.SerializedByteString
    }
    deriving (Show, Eq, Ord, Generic)

instance Aeson.ToJSON SerializedResponse

instance Aeson.FromJSON SerializedResponse

data ResponseChunk
    = ResponseChunk SBS.SerializedByteString
    | ResponseEnd
    | ResponseError Text
    deriving (Show, Eq, Ord, Generic)

instance Aeson.ToJSON ResponseChunk

instance Aeson.FromJSON ResponseChunk

serializeResponse ::
    Servant.Client.Core.Response ->
    SerializedResponse
serializeResponse coreResponse =
    SerializedResponse
        { responseStatusCode =
            serializeStatusCode $
            Servant.Client.Core.responseStatusCode coreResponse
        , responseHeaders =
            fmap
                (\(name, value) ->
                    ( SBS.SerializedByteString $ CI.foldedCase name
                    , SBS.SerializedByteString value
                    )
                ) $
            Servant.Client.Core.responseHeaders coreResponse
        , responseHttpVersion =
            case Servant.Client.Core.responseHttpVersion coreResponse of
                Network.HTTP.Types.HttpVersion v1 v2 -> (v1, v2)
        , responseBody =
            Just $
                SBS.SerializedByteString $
                BS.Lazy.toStrict $
                Servant.Client.Core.responseBody coreResponse
        }

restoreResponse ::
    SerializedResponse ->
    (Maybe SBS.SerializedByteString -> a) ->
    Servant.Client.Core.ResponseF a
restoreResponse serialResponse handleBody =
    Servant.Client.Streaming.Response
        { Servant.Client.Streaming.responseStatusCode =
            restoreStatusCode $
            responseStatusCode serialResponse
        , Servant.Client.Streaming.responseHeaders =
            fmap
                (\(name, value) ->
                    ( CI.mk $ SBS.getSerializedByteString name
                    , SBS.getSerializedByteString value
                    )
                ) $
            responseHeaders serialResponse
        , Servant.Client.Streaming.responseHttpVersion =
            case responseHttpVersion serialResponse of
                (v1, v2) -> Network.HTTP.Types.HttpVersion v1 v2
        , Servant.Client.Streaming.responseBody =
            handleBody $ responseBody serialResponse
        }

makeSerializedStreamingResponse ::
    Record.RecordTrace ->
    Text ->
    Servant.Client.Streaming.StreamingResponse ->
    (SerializedResponse, Servant.Client.Streaming.StreamingResponse)
makeSerializedStreamingResponse rt reqId coreResponse =
    (serialized, updatedResponse)
  where
    serialized =
        SerializedResponse
            { responseStatusCode =
                serializeStatusCode $
                Servant.Client.Core.responseStatusCode coreResponse
            , responseHeaders =
                fmap
                    (\(name, value) ->
                        ( SBS.SerializedByteString $ CI.foldedCase name
                        , SBS.SerializedByteString value
                        )
                    ) $
                Servant.Client.Core.responseHeaders coreResponse
            , responseHttpVersion =
                case Servant.Client.Core.responseHttpVersion coreResponse of
                    Network.HTTP.Types.HttpVersion v1 v2 -> (v1, v2)
            , responseBody =
                Nothing
            }
    updatedResponse =
        coreResponse
            { Servant.Client.Core.responseBody =
                traceResponseSourceT rt reqId $
                Servant.Client.Core.responseBody coreResponse
            }

replayRestoreStreamingResponse ::
    Text ->
    SerializedResponse ->
    Record.Replayer Servant.Client.Streaming.StreamingResponse
replayRestoreStreamingResponse reqId serialResponse =
    Record.Replayer $ \rt ->
        pure $ Right $
            restoreResponse
                serialResponse
                (restoreResponseStreamingBody rt reqId)

consolidateResponse ::
    Servant.Client.Streaming.StreamingResponse ->
    Record.Replayer Servant.Client.Core.Response
consolidateResponse streamingResponse = do
    eiChunks <-
        Record.replayerLiftIO $ runExceptT $
            Servant.Types.SourceT.runSourceT $
                Servant.Client.Core.responseBody streamingResponse
    case eiChunks of
        Left err ->
            throwM $ ErrorCall err
        Right chunks ->
            pure $ streamingResponse
                { Servant.Client.Core.responseBody =
                    BS.Lazy.fromChunks chunks
                }

restoreStrippedResponseBody ::
    Maybe SBS.SerializedByteString ->
    BS.Lazy.ByteString
restoreStrippedResponseBody Nothing =
    ""
restoreStrippedResponseBody (Just sbs) =
    BS.Lazy.fromStrict $
    SBS.getSerializedByteString sbs

restoreResponseStreamingBody ::
    Record.ReplayContext ->
    Text ->
    Maybe SBS.SerializedByteString ->
    Servant.Types.SourceT.SourceT IO BS.ByteString
restoreResponseStreamingBody rt reqId Nothing =
    replayRestoreResponseSourceT  rt reqId
restoreResponseStreamingBody _ _ (Just sbs) =
    Servant.Types.SourceT.fromStepT $
        Servant.Types.SourceT.Yield (SBS.getSerializedByteString sbs) $
        Servant.Types.SourceT.Stop

traceResponseSourceT ::
    Record.RecordTrace ->
    Text ->
    Servant.Types.SourceT.SourceT IO BS.ByteString ->
    Servant.Types.SourceT.SourceT IO BS.ByteString
traceResponseSourceT rt reqId = Servant.Types.SourceT.mapStepT (go 0)
  where
    go i Servant.Types.SourceT.Stop =
        Servant.Types.SourceT.Effect $ do
            Record.recordTraceEventRequestResult rt
                (ServantClientRequestResponseChunk reqId i)
                ResponseEnd
            pure Servant.Types.SourceT.Stop
    go i (Servant.Types.SourceT.Error msg) =
        Servant.Types.SourceT.Effect $ do
            Record.recordTraceEventRequestResult rt
                (ServantClientRequestResponseChunk reqId i)
                (ResponseError (Text.pack msg))
            pure (Servant.Types.SourceT.Error msg)
    go i (Servant.Types.SourceT.Skip next) =
        Servant.Types.SourceT.Skip (go i next)
    go i (Servant.Types.SourceT.Yield bs next) =
        Servant.Types.SourceT.Yield bs $
        Servant.Types.SourceT.Effect $ do
            Record.recordTraceEventRequestResult rt
                (ServantClientRequestResponseChunk reqId i)
                (ResponseChunk (SBS.SerializedByteString bs))
            pure (go (i+1) next)
    go i (Servant.Types.SourceT.Effect m) =
        Servant.Types.SourceT.Effect (go i <$> m)

replayRestoreResponseSourceT ::
    Record.ReplayContext ->
    Text ->
    Servant.Types.SourceT.SourceT IO BS.ByteString
replayRestoreResponseSourceT rt reqId =
    Servant.Types.SourceT.fromStepT (go 0)
  where
    go i =
        Servant.Types.SourceT.Effect $ do
            eiChunkValue <-
                flip Record.runReplayer rt $
                    Record.replayTraceEventRequest
                        (ServantClientRequestResponseChunk reqId i)
            case eiChunkValue of
                Left ex ->
                    throwM ex
                Right ResponseEnd ->
                    pure Servant.Types.SourceT.Stop
                Right (ResponseChunk sbs) ->
                    pure $
                        Servant.Types.SourceT.Yield
                            (SBS.getSerializedByteString sbs)
                            (go (i + 1))
                Right (ResponseError msg) ->
                    pure $ Servant.Types.SourceT.Error (Text.unpack msg)



data SerializedClientError
    = FailureResponse Text SerializedRequest SerializedResponse
    | DecodeFailure Text SerializedResponse
    | UnsupportedContentType SBS.SerializedByteString SerializedResponse
    | InvalidContentTypeHeader SerializedResponse
    | HttpException SerializedHttpException
    | ConnectionError
    deriving (Show, Eq, Ord, Generic)

instance Aeson.ToJSON SerializedClientError

instance Aeson.FromJSON SerializedClientError

serializeClientError ::
    Servant.Client.ClientError ->
    SerializedClientError
serializeClientError (Servant.Client.FailureResponse req resp) = do
    let (baseUrlText, serializedReq) =
            serializeStrippedRequest req
    let serializedResp =
            serializeResponse resp
    FailureResponse baseUrlText serializedReq serializedResp
serializeClientError (Servant.Client.DecodeFailure msg resp) = do
    let serializedResp =
            serializeResponse resp
    DecodeFailure msg serializedResp
serializeClientError (Servant.Client.UnsupportedContentType mt resp) = do
    let serializedMediaType =
            SBS.SerializedByteString $
            Network.HTTP.Media.renderHeader mt
    let serializedResp =
            serializeResponse resp
    UnsupportedContentType serializedMediaType serializedResp
serializeClientError (Servant.Client.InvalidContentTypeHeader resp) = do
    let serializedResp =
            serializeResponse resp
    InvalidContentTypeHeader serializedResp
serializeClientError (Servant.Client.ConnectionError ex)
    | Just httpException <- fromException ex =
        HttpException
            (serializeHttpException httpException)
    | otherwise =
        ConnectionError

restoreClientError ::
    SerializedClientError ->
    Servant.Client.ClientError
restoreClientError (FailureResponse baseUrlText sreq sresp) = do
    let !req = restoreStrippedRequest baseUrlText sreq
    let !resp = restoreResponse sresp restoreStrippedResponseBody
    Servant.Client.FailureResponse req resp
restoreClientError (DecodeFailure msg sresp) = do
    let !resp = restoreResponse sresp restoreStrippedResponseBody
    Servant.Client.DecodeFailure msg resp
restoreClientError (UnsupportedContentType smt sresp) = do
    let !mediaType = parseMediaType smt
    let !resp = restoreResponse sresp restoreStrippedResponseBody
    Servant.Client.UnsupportedContentType mediaType resp
  where
    parseMediaType =
        maybe (error "bad accept") id .
        Network.HTTP.Media.parseAccept .
        SBS.getSerializedByteString
restoreClientError (InvalidContentTypeHeader sresp) = do
    let !resp = restoreResponse sresp restoreStrippedResponseBody
    Servant.Client.InvalidContentTypeHeader resp
restoreClientError (HttpException httpException) = do
    Servant.Client.ConnectionError $
        toException $ restoreHttpException httpException
restoreClientError ConnectionError = do
    Servant.Client.ConnectionError $ toException $ ErrorCall "connection error"



data SerializedHttpException
    = HttpStatusCodeException
        (Int, SBS.SerializedByteString) SBS.SerializedByteString
    | HttpTooManyRedirects
    | HttpOverlongHeaders
    | HttpResponseTimeout
    | HttpConnectionTimeout
    | HttpConnectionFailure
    | HttpInvalidStatusLine SBS.SerializedByteString
    | HttpInvalidHeader SBS.SerializedByteString
    | HttpInvalidRequestHeader SBS.SerializedByteString
    | HttpInternalException
    | HttpProxyConnectException
        SBS.SerializedByteString Int (Int, SBS.SerializedByteString)
    | HttpNoResponseDataReceived
    | HttpTlsNotSupported
    | HttpWrongRequestBodyStreamSize Word64 Word64
    | HttpResponseBodyTooShort Word64 Word64
    | HttpInvalidChunkHeaders
    | HttpIncompleteHeaders
    | HttpInvalidDestinationHost SBS.SerializedByteString
    | HttpZlibException Int
    | HttpInvalidProxyEnvironmentVariable Text Text
    | HttpConnectionClosed
    | HttpInvalidProxySettings Text
    | HttpInvalidUrlException String String
    deriving (Show, Eq, Ord, Generic)

instance Aeson.ToJSON SerializedHttpException

instance Aeson.FromJSON SerializedHttpException

serializeHttpException ::
    Network.HTTP.Client.HttpException ->
    SerializedHttpException
serializeHttpException (Network.HTTP.Client.HttpExceptionRequest _ content) =
    case content of
        Network.HTTP.Client.StatusCodeException resp body ->
            HttpStatusCodeException
                (serializeStatusCode $ Network.HTTP.Client.responseStatus resp)
                (SBS.SerializedByteString body)
        Network.HTTP.Client.TooManyRedirects _ ->
            HttpTooManyRedirects
        Network.HTTP.Client.OverlongHeaders ->
            HttpOverlongHeaders
        Network.HTTP.Client.ResponseTimeout ->
            HttpResponseTimeout
        Network.HTTP.Client.ConnectionTimeout ->
            HttpConnectionTimeout
        Network.HTTP.Client.ConnectionFailure _ex ->
            HttpConnectionFailure
        Network.HTTP.Client.InvalidStatusLine line ->
            HttpInvalidStatusLine
                (SBS.SerializedByteString line)
        Network.HTTP.Client.InvalidHeader header ->
            HttpInvalidHeader
                (SBS.SerializedByteString header)
        Network.HTTP.Client.InvalidRequestHeader header ->
            HttpInvalidRequestHeader
                (SBS.SerializedByteString header)
        Network.HTTP.Client.InternalException _ ->
            HttpInternalException
        Network.HTTP.Client.ProxyConnectException host port status ->
            HttpProxyConnectException
                (SBS.SerializedByteString host)
                port
                (serializeStatusCode status)
        Network.HTTP.Client.NoResponseDataReceived ->
            HttpNoResponseDataReceived
        Network.HTTP.Client.TlsNotSupported ->
            HttpTlsNotSupported
        Network.HTTP.Client.WrongRequestBodyStreamSize expected actual ->
            HttpWrongRequestBodyStreamSize expected actual
        Network.HTTP.Client.ResponseBodyTooShort expected actual ->
            HttpResponseBodyTooShort expected actual
        Network.HTTP.Client.InvalidChunkHeaders ->
            HttpInvalidChunkHeaders
        Network.HTTP.Client.IncompleteHeaders ->
            HttpIncompleteHeaders
        Network.HTTP.Client.InvalidDestinationHost host ->
            HttpInvalidDestinationHost
                (SBS.SerializedByteString host)
        Network.HTTP.Client.HttpZlibException (Zlib.ZlibException code) ->
            HttpZlibException code
        Network.HTTP.Client.InvalidProxyEnvironmentVariable name value ->
            HttpInvalidProxyEnvironmentVariable name value
        Network.HTTP.Client.ConnectionClosed ->
            HttpConnectionClosed
        Network.HTTP.Client.InvalidProxySettings msg ->
            HttpInvalidProxySettings msg
serializeHttpException (Network.HTTP.Client.InvalidUrlException url msg) =
    HttpInvalidUrlException url msg

restoreHttpException ::
    SerializedHttpException ->
    Network.HTTP.Client.HttpException
restoreHttpException ex =
    case ex of
        HttpStatusCodeException status body ->
            exceptionRequest $
                Network.HTTP.Client.StatusCodeException
                    (defaultResponse $ restoreStatusCode status)
                    (SBS.getSerializedByteString body)
        HttpTooManyRedirects ->
            exceptionRequest $
                Network.HTTP.Client.TooManyRedirects []
        HttpOverlongHeaders ->
            exceptionRequest $
                Network.HTTP.Client.OverlongHeaders
        HttpResponseTimeout ->
            exceptionRequest $
                Network.HTTP.Client.ResponseTimeout
        HttpConnectionTimeout ->
            exceptionRequest $
                Network.HTTP.Client.ConnectionTimeout
        HttpConnectionFailure ->
            exceptionRequest $
                Network.HTTP.Client.ConnectionFailure
                    (toException $ ErrorCall "connection failure")
        HttpInvalidStatusLine line ->
            exceptionRequest $
                Network.HTTP.Client.InvalidStatusLine
                    (SBS.getSerializedByteString line)
        HttpInvalidHeader header ->
            exceptionRequest $
                Network.HTTP.Client.InvalidHeader
                    (SBS.getSerializedByteString header)
        HttpInvalidRequestHeader header ->
            exceptionRequest $
                Network.HTTP.Client.InvalidRequestHeader
                    (SBS.getSerializedByteString header)
        HttpInternalException ->
            exceptionRequest $
                Network.HTTP.Client.InternalException
                    (toException $ ErrorCall "internal exception")
        HttpProxyConnectException host port status ->
            exceptionRequest $
                Network.HTTP.Client.ProxyConnectException
                    (SBS.getSerializedByteString host)
                    port
                    (restoreStatusCode status)
        HttpNoResponseDataReceived ->
            exceptionRequest $
                Network.HTTP.Client.NoResponseDataReceived
        HttpTlsNotSupported ->
            exceptionRequest $
                Network.HTTP.Client.TlsNotSupported
        HttpWrongRequestBodyStreamSize expected actual ->
            exceptionRequest $
                Network.HTTP.Client.WrongRequestBodyStreamSize expected actual
        HttpResponseBodyTooShort expected actual ->
            exceptionRequest $
                Network.HTTP.Client.ResponseBodyTooShort expected actual
        HttpInvalidChunkHeaders ->
            exceptionRequest $
                Network.HTTP.Client.InvalidChunkHeaders
        HttpIncompleteHeaders ->
            exceptionRequest $
                Network.HTTP.Client.IncompleteHeaders
        HttpInvalidDestinationHost host ->
            exceptionRequest $
                Network.HTTP.Client.InvalidDestinationHost
                    (SBS.getSerializedByteString host)
        HttpZlibException code ->
            exceptionRequest $
                Network.HTTP.Client.HttpZlibException (Zlib.ZlibException code)
        HttpInvalidProxyEnvironmentVariable name value ->
            exceptionRequest $
                Network.HTTP.Client.InvalidProxyEnvironmentVariable name value
        HttpConnectionClosed ->
            exceptionRequest $
                Network.HTTP.Client.ConnectionClosed
        HttpInvalidProxySettings msg ->
            exceptionRequest $
                Network.HTTP.Client.InvalidProxySettings msg
        HttpInvalidUrlException url msg ->
            Network.HTTP.Client.InvalidUrlException url msg
  where
    exceptionRequest =
        Network.HTTP.Client.HttpExceptionRequest
            Network.HTTP.Client.defaultRequest
    defaultResponse status =
        Network.HTTP.Client.Internal.Response
            { Network.HTTP.Client.Internal.responseStatus =
                status
            , Network.HTTP.Client.Internal.responseVersion =
                Network.HTTP.Types.http11
            , Network.HTTP.Client.Internal.responseHeaders =
                []
            , Network.HTTP.Client.Internal.responseBody =
                ()
            , Network.HTTP.Client.Internal.responseCookieJar =
                mempty
            , Network.HTTP.Client.Internal.responseClose' =
                Network.HTTP.Client.Internal.ResponseClose $ do
                    pure ()
            }



serializeStatusCode ::
    Network.HTTP.Types.Status ->
    (Int, SBS.SerializedByteString)
serializeStatusCode status =
    ( Network.HTTP.Types.statusCode status
    , SBS.SerializedByteString $
        Network.HTTP.Types.statusMessage status
    )

restoreStatusCode ::
    (Int, SBS.SerializedByteString) ->
    Network.HTTP.Types.Status
restoreStatusCode (sn, sb) =
    Network.HTTP.Types.mkStatus
        sn
        (SBS.getSerializedByteString sb)
