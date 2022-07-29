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
    (
      ClientFC.ClientFC
    , ClientFC.Client
    , ClientFC.StreamingClient
    , ClientFC.IsStreaming (..)
    , ClientFC.client
    , ClientFC.streamingClient
    , MonadServantClient (..)
    , MonadServantClientStreaming (..)
    )
where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.IORef
import Data.Maybe
import qualified Control.Monad.Codensity as Codensity
import qualified Control.Record as Record
import qualified Control.Reducible as Reducible
import qualified Network.HTTP.Types
import qualified Servant.Client.Core
import qualified Servant.Client.Core.RunClient
import qualified Servant.Client.Internal.HttpClient.Streaming
import qualified Servant.Client.Streaming
import qualified Servant.Client.Traced.ClientFC as ClientFC
import qualified Servant.Client.Traced.ReqId as ReqId
import qualified Servant.Client.Traced.Serialization as Serialization

class (Monad m) => MonadServantClient m where
    runClientM ::
        ClientFC.Client a ->
        m (Either Servant.Client.Core.ClientError a)

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
        ClientFC.ClientFC s a ->
        (Either Servant.Client.Core.ClientError a -> m b) ->
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



instance
    {-# OVERLAPPING #-}
    MonadServantClient (ReaderT Servant.Client.Streaming.ClientEnv IO)
  where
    runClientM cfc = withClientM cfc pure

-- TODO: add a function like "liftBracket" into reducible

instance
    {-# OVERLAPPING #-}
    MonadServantClientStreaming (ReaderT Servant.Client.Streaming.ClientEnv IO)
  where
    withClientM cfc inner = ReaderT $ \cenv ->
        Servant.Client.Streaming.withClientM
            (runClientFCStreaming cfc)
            cenv
            (\result -> runReaderT (inner result) cenv)

runClientFCStreaming ::
    ClientFC.ClientFC s a ->
    Servant.Client.Streaming.ClientM a
runClientFCStreaming cfc = do
    ClientFC.runClientFC cfc
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
    ClientFC.ClientFC s a ->
    ClientFC.ClientFC s a
wrapRecordingClientFC rt cfc =
    ClientFC.ClientFC $ \onThrow onReq onWithSReq onIO outerCont ->
        ClientFC.runClientFC cfc
            onThrow
            (wrapRecordingOnReq rt onIO onReq)
            (wrapRecordingOnWithSReq rt onIO onWithSReq)
            onIO
            outerCont

wrapRecordingOnReq ::
    Record.RecordTrace ->
    (IO q -> q) ->
    ClientFC.ClientFCReqHandler q ->
    ClientFC.ClientFCReqHandler q
wrapRecordingOnReq rt onIO onReq =
    \mbStatusList coreRequest innerCatch innerCont ->
        onIO $ do
            reqId <- recordRequestStart rt mbStatusList coreRequest
            pure $
                onReq
                    mbStatusList
                    (Serialization.wrapRecordingRequest rt reqId coreRequest)
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
    ClientFC.ClientFCWithSReqHandler q ->
    ClientFC.ClientFCWithSReqHandler q
wrapRecordingOnWithSReq rt onIO onWithSReq =
    \coreRequest act innerCatch innerCont ->
        onIO $ do
            reqId <- recordRequestStart rt Nothing coreRequest
            pure $
                onWithSReq
                    (Serialization.wrapRecordingRequest rt reqId coreRequest)
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
    IO ReqId.ReqId
recordRequestStart rt mbStatusList coreRequest = do
    reqId <- ReqId.generateReqId
    let serializedRequest =
            Serialization.serializeRequest coreRequest
    Record.recordTraceEventRequestResult rt
        (Serialization.ServantClientRequestStart serializedRequest)
        reqId
    Record.recordTraceEventSyncPoint rt
        (Serialization.ServantClientSyncPointRequest serializedRequest)
    Record.recordTraceEventEffect rt
        (Serialization.ServantClientEffectRequest reqId serializedRequest)
    forM_ mbStatusList $ \statusList -> do
        Record.recordTraceEventEffect rt
            (Serialization.ServantClientEffectSetAcceptStatus
                reqId
                (fmap Serialization.serializeStatusCode statusList)
            )
    pure reqId

recordClientError ::
    Record.RecordTrace ->
    ReqId.ReqId ->
    Servant.Client.Core.ClientError ->
    IO ()
recordClientError rt reqId clientError = do
    Record.recordTraceEventRequestResult rt
        (Serialization.ServantClientRequestOutcome reqId)
        (Serialization.RequestOutcomeError $
            Serialization.serializeClientError clientError
        )

recordSimpleResponse ::
    Record.RecordTrace ->
    ReqId.ReqId ->
    Servant.Client.Core.Response ->
    IO ()
recordSimpleResponse rt reqId response = do
    let serializedResponse =
            Serialization.serializeResponse response
    Record.recordTraceEventRequestResult rt
        (Serialization.ServantClientRequestOutcome reqId)
        (Serialization.RequestOutcomeResponse serializedResponse)

recordStreamingResponse ::
    Record.RecordTrace ->
    ReqId.ReqId ->
    Servant.Client.Streaming.StreamingResponse ->
    IO Servant.Client.Streaming.StreamingResponse
recordStreamingResponse rt reqId streamingResponse = do
    let (serializedResponse, updatedResponse) =
            Serialization.makeSerializedStreamingResponse
                rt reqId streamingResponse
    Record.recordTraceEventRequestResult rt
        (Serialization.ServantClientRequestOutcome reqId)
        (Serialization.RequestOutcomeResponse serializedResponse)
    pure updatedResponse



fixedUnwrappedWithStreamingRequest ::
    Servant.Client.Core.Request ->
    (Servant.Client.Streaming.StreamingResponse -> IO a) ->
    Servant.Client.Streaming.ClientEnv ->
    (Either Servant.Client.Core.ClientError a -> IO r) ->
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
                            (Servant.Client.Core.ConnectionError ex)
                            (fromException ex)
                handlingRequest <- readIORef handlingRequestIORef
                if handlingRequest
                    then restore $ cont (Left clientError)
                    else throwM clientError

unwrappedRunRequestAcceptStatus ::
    Maybe [Network.HTTP.Types.Status] ->
    Servant.Client.Core.Request ->
    Servant.Client.Streaming.ClientEnv ->
    (Either Servant.Client.Core.ClientError Servant.Client.Core.Response -> IO r) ->
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
    (Either Servant.Client.Core.ClientError a -> IO r) ->
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
    (Either Servant.Client.Core.ClientError a -> IO r) ->
    IO r
unwrapClientM m env =
    Codensity.runCodensity $
    runExceptT $
    flip runReaderT env $
    Servant.Client.Internal.HttpClient.Streaming.unClientM m

wrapClientM ::
    (forall r.
        Servant.Client.Streaming.ClientEnv ->
        (Either Servant.Client.Core.ClientError a -> IO r) ->
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
    ClientFC.ClientFC s a ->
    (Either Servant.Client.Core.ClientError a -> Record.Replayer r) ->
    Record.Replayer r
withClientFCReplay cfc outerCont = do
    ClientFC.runClientFC cfc
        (outerCont . Left)
        (\mbStatusList coreRequest innerCatch innerCont -> do
            replayRequest mbStatusList coreRequest
                (innerCatch . Serialization.restoreClientError)
                (\streamingResponse -> do
                    response <- Serialization.consolidateResponse streamingResponse
                    innerCont response
                )
        )
        (\coreRequest act innerCatch innerCont -> do
            replayRequest Nothing coreRequest
                (innerCatch . Serialization.restoreClientError)
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
    (Serialization.SerializedClientError -> Record.Replayer r) ->
    (Servant.Client.Streaming.StreamingResponse -> Record.Replayer r) ->
    Record.Replayer r
replayRequest mbStatusList coreRequest onClientError onResponse = do
    let serializedRequest =
            Serialization.serializeRequest coreRequest
    reqId <-
        Record.replayTraceEventRequest
            (Serialization.ServantClientRequestStart serializedRequest)
    Record.replayTraceEventSyncPoint
        (Serialization.ServantClientSyncPointRequest serializedRequest)
    Record.replayTraceEventEffect
        (Serialization.ServantClientEffectRequest reqId serializedRequest)
    forM_ mbStatusList $ \statusList -> do
        Record.replayTraceEventEffect
            (Serialization.ServantClientEffectSetAcceptStatus
                reqId
                (fmap Serialization.serializeStatusCode statusList)
            )
    Serialization.replayDrainRequest reqId coreRequest
    requestOutcome <-
        Record.replayTraceEventRequest
            (Serialization.ServantClientRequestOutcome reqId)
    case requestOutcome of
        Serialization.RequestOutcomeError clientError ->
            onClientError clientError
        Serialization.RequestOutcomeResponse responseHeader -> do
            streamingResponse <-
                Serialization.replayRestoreStreamingResponse reqId responseHeader
            onResponse streamingResponse
