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
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Data.ByteString.Lazy.Char8 as BS.Lazy.Char8
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

data IsStreaming = Streaming | NoStreaming

newtype TracedClientM (s :: IsStreaming) a = TracedClientM
    { runTracedClientM ::
        Record.RecordTrace ->
        Servant.Client.Streaming.ClientM a
    }
    deriving (Functor, Applicative, Monad)
        via (ReaderT Record.RecordTrace Servant.Client.Streaming.ClientM)

class (Monad m) => MonadServantClient m where
    withClientM ::
        TracedClientM s a ->
        (Either Servant.Client.ClientError a -> m b) ->
        m b

runClientM ::
    (MonadServantClient m) =>
    TracedClientM 'NoStreaming a ->
    m (Either Servant.Client.ClientError a)
runClientM client =
    withClientM client pure

instance
    MonadServantClient
        (ReaderT (Record.RecordTrace, Servant.Client.ClientEnv) IO)
  where
    withClientM client inner =
        ReaderT $ \(rt, cenv) ->
            Servant.Client.Streaming.withClientM
                (runTracedClientM client rt)
                cenv
                (\result -> runReaderT (inner result) (rt, cenv))



instance Record.TraceDomain MonadServantClient where
    data TraceDomainRequest MonadServantClient
        = ServantClientRequestRequest SerializedRequest
        | ServantClientRequestResponseHeader Text
        | ServantClientRequestResponseChunk Text Int
        deriving (Show, Eq, Ord, Generic)
    newtype TraceDomainError MonadServantClient
        = ServantClientError SerializedClientError
        deriving (Show, Eq, Ord, Generic)
    data TraceDomainEffect MonadServantClient
        = ServantClientEffectSetAcceptStatus [(Int, SBS.SerializedByteString)]
        | ServantClientEffectRequest Text SerializedRequest
        | ServantClientEffectRequestChunk Text SBS.SerializedByteString
        deriving (Show, Eq, Ord, Generic)
    data TraceDomainSyncPoint MonadServantClient
        = ServantClientSyncPointRequest SerializedRequest
        deriving (Show, Eq, Ord, Generic)
    traceDomainErrorFromException (Just ex)
        | Just clientError <- fromException ex =
            Just $
                ServantClientError $
                makeSerializedClientError clientError
    traceDomainErrorFromException _ = Nothing
    traceDomainErrorToException (ServantClientError clientError) =
        toException $
        restoreSerializedClientError clientError

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



instance Servant.Client.Core.RunClient (TracedClientM s) where
    runRequestAcceptStatus mbStatusList coreRequest = TracedClientM $ \rt -> do
        wrapClientM $ \env cont -> do
            reqId <- RandomId.generateRandomId
            let (serializedRequest, updatedRequest) =
                    makeSerializedRequest rt reqId coreRequest
            Record.recordTraceEventRequestResult rt
                (ServantClientRequestRequest serializedRequest)
                reqId
            Record.recordTraceEventSyncPoint rt
                (ServantClientSyncPointRequest serializedRequest)
            Record.recordTraceEventEffect rt
                (ServantClientEffectRequest reqId serializedRequest)
            forM_ mbStatusList $ \statusList -> do
                Record.recordTraceEventEffect rt
                    (ServantClientEffectSetAcceptStatus $
                        fmap serializeStatusCode statusList
                    )
            unwrappedRunRequestAcceptStatus
                mbStatusList
                updatedRequest
                env
                (\eiResponse -> do
                    case eiResponse of
                        Left clientError -> do
                            Record.recordTraceEventRequestError rt
                                (ServantClientRequestResponseHeader reqId)
                                (ServantClientError $
                                    makeSerializedClientError clientError
                                )
                        Right response -> do
                            let serializedResponse =
                                    makeSerializedResponse response
                            Record.recordTraceEventRequestResult rt
                                (ServantClientRequestResponseHeader reqId)
                                serializedResponse
                    cont eiResponse
                )
    throwClientError clientError = TracedClientM $ \_rt -> do
        Servant.Client.Core.throwClientError clientError

instance
    Servant.Client.Core.RunClient.RunStreamingClient (TracedClientM 'Streaming)
  where
    withStreamingRequest coreRequest inner = TracedClientM $ \rt -> do
        wrapClientM $ \env cont -> do
            reqId <- RandomId.generateRandomId
            let (serializedRequest, updatedRequest) =
                    makeSerializedRequest rt reqId coreRequest
            Record.recordTraceEventRequestResult rt
                (ServantClientRequestRequest serializedRequest)
                reqId
            Record.recordTraceEventSyncPoint rt
                (ServantClientSyncPointRequest serializedRequest)
            Record.recordTraceEventEffect rt
                (ServantClientEffectRequest reqId serializedRequest)
            fixedUnwrappedWithStreamingRequest
                updatedRequest
                (\streamingResponse -> do
                    let (serializedResponse, updatedResponse) =
                            makeSerializedStreamingResponse
                                rt reqId streamingResponse
                    Record.recordTraceEventRequestResult rt
                        (ServantClientRequestResponseHeader reqId)
                        serializedResponse
                    inner updatedResponse
                )
                env
                (\eiResult -> do
                    case eiResult of
                        Left clientError -> do
                            Record.recordTraceEventRequestError rt
                                (ServantClientRequestResponseHeader reqId)
                                (ServantClientError $
                                    makeSerializedClientError clientError
                                )
                        _ ->
                            pure ()
                    cont eiResult
                )

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



makeSerializedStrippedRequest ::
    Servant.Client.Core.RequestF () (Servant.Client.BaseUrl, BS.ByteString) ->
    (Text, SerializedRequest)
makeSerializedStrippedRequest coreRequest =
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

restoreSerializedStrippedRequest ::
    Text ->
    SerializedRequest ->
    Servant.Client.Core.RequestF () (Servant.Client.BaseUrl, BS.ByteString)
restoreSerializedStrippedRequest baseUrlText serialRequest =
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

makeSerializedRequest ::
    Record.RecordTrace ->
    Text ->
    Servant.Client.Core.Request ->
    (SerializedRequest, Servant.Client.Core.Request)
makeSerializedRequest rt reqId coreRequest =
    (serialized, updatedRequest)
  where
    (mbSerialBody, mbUpdatedBody) =
        case Servant.Client.Core.requestBody coreRequest of
            Nothing -> (Nothing, Nothing)
            Just (coreContents, coreMediaType) ->
                ( Just (serialMediaType, serialContents)
                , Just (updatedContents, coreMediaType)
                )
              where
                (serialContents, updatedContents) =
                    makeSerializedRequestBody rt reqId coreContents
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
                mbSerialBody
            }
    updatedRequest =
        coreRequest
            { Servant.Client.Core.requestBody = mbUpdatedBody
            }

makeSerializedRequestBody ::
    Record.RecordTrace ->
    Text ->
    Servant.Client.Core.RequestBody ->
    (Maybe SBS.SerializedByteString, Servant.Client.Core.RequestBody)
makeSerializedRequestBody rt reqId coreBody =
    case coreBody of
        Servant.Client.Core.RequestBodyLBS lbs ->
            ( Just $ SBS.SerializedByteString $ BS.Lazy.toStrict lbs
            , coreBody
            )
        Servant.Client.Core.RequestBodyBS bs ->
            ( Just $ SBS.SerializedByteString bs
            , coreBody
            )
        Servant.Client.Core.RequestBodySource sio ->
            ( Nothing
            , Servant.Client.Core.RequestBodySource $
                traceRequestSourceT rt reqId sio
            )

traceRequestSourceT ::
    Record.RecordTrace ->
    Text ->
    Servant.Types.SourceT.SourceT IO BS.Lazy.ByteString ->
    Servant.Types.SourceT.SourceT IO BS.Lazy.ByteString
traceRequestSourceT rt reqId = Servant.Types.SourceT.mapStepT go
  where
    go Servant.Types.SourceT.Stop =
        Servant.Types.SourceT.Stop
    go (Servant.Types.SourceT.Error msg) =
        Servant.Types.SourceT.Error msg
    go (Servant.Types.SourceT.Skip next) =
        Servant.Types.SourceT.Skip (go next)
    go (Servant.Types.SourceT.Yield lbs next) =
        Servant.Types.SourceT.Yield lbs $
        Servant.Types.SourceT.Effect $ do
            Record.recordTraceEventEffect rt
                    (ServantClientEffectRequestChunk
                        reqId
                        (SBS.SerializedByteString $ BS.Lazy.toStrict lbs)
                    )
            pure (go next)
    go (Servant.Types.SourceT.Effect m) =
        Servant.Types.SourceT.Effect (go <$> m)



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

makeSerializedResponse ::
    Servant.Client.Core.Response ->
    SerializedResponse
makeSerializedResponse coreResponse =
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

restoreSerializedResponse ::
    SerializedResponse ->
    Servant.Client.Core.Response
restoreSerializedResponse serialResponse =
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
            case responseBody serialResponse of
                Nothing -> ""
                Just sbs ->
                    BS.Lazy.fromStrict $
                    SBS.getSerializedByteString sbs
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

makeSerializedClientError ::
    Servant.Client.ClientError ->
    SerializedClientError
makeSerializedClientError (Servant.Client.FailureResponse req resp) = do
    let (baseUrlText, serializedReq) =
            makeSerializedStrippedRequest req
    let serializedResp =
            makeSerializedResponse resp
    FailureResponse baseUrlText serializedReq serializedResp
makeSerializedClientError (Servant.Client.DecodeFailure msg resp) = do
    let serializedResp =
            makeSerializedResponse resp
    DecodeFailure msg serializedResp
makeSerializedClientError (Servant.Client.UnsupportedContentType mt resp) = do
    let serializedMediaType =
            SBS.SerializedByteString $
            Network.HTTP.Media.renderHeader mt
    let serializedResp =
            makeSerializedResponse resp
    UnsupportedContentType serializedMediaType serializedResp
makeSerializedClientError (Servant.Client.InvalidContentTypeHeader resp) = do
    let serializedResp =
            makeSerializedResponse resp
    InvalidContentTypeHeader serializedResp
makeSerializedClientError (Servant.Client.ConnectionError ex)
    | Just httpException <- fromException ex =
        HttpException
            (makeSerializedHttpException httpException)
    | otherwise =
        ConnectionError

restoreSerializedClientError ::
    SerializedClientError ->
    Servant.Client.ClientError
restoreSerializedClientError (FailureResponse baseUrlText sreq sresp) = do
    let !req = restoreSerializedStrippedRequest baseUrlText sreq
    let !resp = restoreSerializedResponse sresp
    Servant.Client.FailureResponse req resp
restoreSerializedClientError (DecodeFailure msg sresp) = do
    let !resp = restoreSerializedResponse sresp
    Servant.Client.DecodeFailure msg resp
restoreSerializedClientError (UnsupportedContentType smt sresp) = do
    let !mediaType = parseMediaType smt
    let !resp = restoreSerializedResponse sresp
    Servant.Client.UnsupportedContentType mediaType resp
  where
    parseMediaType =
        maybe (error "bad accept") id .
        Network.HTTP.Media.parseAccept .
        SBS.getSerializedByteString
restoreSerializedClientError (InvalidContentTypeHeader sresp) = do
    let !resp = restoreSerializedResponse sresp
    Servant.Client.InvalidContentTypeHeader resp
restoreSerializedClientError (HttpException httpException) = do
    Servant.Client.ConnectionError $
        toException $ restoreSerializedHttpException httpException
restoreSerializedClientError ConnectionError = do
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

makeSerializedHttpException ::
    Network.HTTP.Client.HttpException ->
    SerializedHttpException
makeSerializedHttpException (Network.HTTP.Client.HttpExceptionRequest _ content) =
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
makeSerializedHttpException (Network.HTTP.Client.InvalidUrlException url msg) =
    HttpInvalidUrlException url msg

restoreSerializedHttpException ::
    SerializedHttpException ->
    Network.HTTP.Client.HttpException
restoreSerializedHttpException ex =
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
