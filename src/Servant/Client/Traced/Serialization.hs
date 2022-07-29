{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Servant.Client.Traced.Serialization
where

import Control.Exception (ErrorCall (..))
import Control.Monad.Catch
import Control.Monad.Except
import Data.Foldable
import Data.Sequence (Seq (..))
import Data.Text (Text)
import Data.Void
import Data.Word
import GHC.Generics
import Servant.Client.Traced.SerializedByteString
import qualified Control.Record as Record
import qualified Data.Aeson as Aeson
import qualified Data.Binary.Builder as Binary.Builder
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Data.CaseInsensitive as CI
import qualified Data.Streaming.Zlib
import qualified Data.Text as Text
import qualified Network.HTTP.Client.Internal
import qualified Network.HTTP.Media
import qualified Network.HTTP.Types
import qualified Servant.Client.Core
import qualified Servant.Client.Streaming
import qualified Servant.Client.Traced.ReqId as ReqId
import qualified Servant.Types.SourceT
import qualified System.IO.Unsafe

{-  Referring to MonadServantClient directly from here would create a circular
    dependency. To avoid it, we use a separate marker type for TraceDomain. -}
data ServantClient

instance Record.TraceDomain ServantClient where
    data TraceDomainRequest ServantClient
        = ServantClientRequestStart SerializedRequest
        | ServantClientRequestOutcome ReqId.ReqId
        | ServantClientRequestResponseChunk ReqId.ReqId Int
        deriving (Show, Eq, Ord, Generic)
    newtype TraceDomainError ServantClient
        = ServantClientError Void
        deriving (Show, Eq, Ord, Generic)
    data TraceDomainEffect ServantClient
        = ServantClientEffectSetAcceptStatus ReqId.ReqId [(Int, SerializedByteString)]
        | ServantClientEffectRequest ReqId.ReqId SerializedRequest
        | ServantClientEffectRequestChunk ReqId.ReqId SerializedByteString
        deriving (Show, Eq, Ord, Generic)
    data TraceDomainSyncPoint ServantClient
        = ServantClientSyncPointRequest SerializedRequest
        deriving (Show, Eq, Ord, Generic)
    traceDomainErrorFromException _ = Nothing
    traceDomainErrorToException (ServantClientError v) = absurd v

instance Aeson.ToJSON (Record.TraceDomainRequest ServantClient) where
    toJSON = Aeson.genericToJSON
        (Record.aesonOptions (Text.length "ServantClientRequest"))
    toEncoding = Aeson.genericToEncoding
        (Record.aesonOptions (Text.length "ServantClientRequest"))

instance Aeson.FromJSON (Record.TraceDomainRequest ServantClient) where
    parseJSON = Aeson.genericParseJSON
        (Record.aesonOptions (Text.length "ServantClientRequest"))

instance Aeson.ToJSON (Record.TraceDomainError ServantClient)

instance Aeson.FromJSON (Record.TraceDomainError ServantClient)

instance Aeson.ToJSON (Record.TraceDomainEffect ServantClient) where
    toJSON = Aeson.genericToJSON
        (Record.aesonOptions (Text.length "ServantClientEffect"))
    toEncoding = Aeson.genericToEncoding
        (Record.aesonOptions (Text.length "ServantClientEffect"))

instance Aeson.FromJSON (Record.TraceDomainEffect ServantClient) where
    parseJSON = Aeson.genericParseJSON
        (Record.aesonOptions (Text.length "ServantClientEffect"))

instance Aeson.ToJSON (Record.TraceDomainSyncPoint ServantClient) where
    toJSON = Aeson.genericToJSON
        (Record.aesonOptions (Text.length "ServantClientSyncPoint"))
    toEncoding = Aeson.genericToEncoding
        (Record.aesonOptions (Text.length "ServantClientSyncPoint"))

instance Aeson.FromJSON (Record.TraceDomainSyncPoint ServantClient) where
    parseJSON = Aeson.genericParseJSON
        (Record.aesonOptions (Text.length "ServantClientSyncPoint"))



data SerializedRequest = SerializedRequest
    { requestHttpVersion ::
        (Int, Int)
    , requestMethod ::
        SerializedByteString
    , requestPath ::
        SerializedByteString
    , requestQueryString ::
        Seq (SerializedByteString, Maybe SerializedByteString)
    , requestAccept ::
        Seq SerializedByteString
    , requestHeaders ::
        Seq (SerializedByteString, SerializedByteString)
    , requestBody ::
        Maybe (SerializedByteString, Maybe SerializedByteString)
    }
    deriving (Show, Eq, Ord, Generic)

instance Aeson.ToJSON SerializedRequest

instance Aeson.FromJSON SerializedRequest

data RequestOutcome
    = RequestOutcomeError SerializedClientError
    | RequestOutcomeResponse SerializedResponse
    deriving (Show, Eq, Ord, Generic)

instance Aeson.ToJSON RequestOutcome

instance Aeson.FromJSON RequestOutcome

data SerializedResponse = SerializedResponse
    { responseStatusCode ::
        (Int, SerializedByteString)
    , responseHeaders ::
        Seq (SerializedByteString, SerializedByteString)
    , responseHttpVersion ::
        (Int, Int)
    , responseBody ::
        Maybe SerializedByteString
    }
    deriving (Show, Eq, Ord, Generic)

instance Aeson.ToJSON SerializedResponse

instance Aeson.FromJSON SerializedResponse

data ResponseChunk
    = ResponseChunk SerializedByteString
    | ResponseEnd
    | ResponseError Text
    deriving (Show, Eq, Ord, Generic)

instance Aeson.ToJSON ResponseChunk

instance Aeson.FromJSON ResponseChunk

data SerializedClientError
    = FailureResponse Text SerializedRequest SerializedResponse
    | DecodeFailure Text SerializedResponse
    | UnsupportedContentType SerializedByteString SerializedResponse
    | InvalidContentTypeHeader SerializedResponse
    | HttpException SerializedHttpException
    | ConnectionError
    deriving (Show, Eq, Ord, Generic)

instance Aeson.ToJSON SerializedClientError

instance Aeson.FromJSON SerializedClientError

data SerializedHttpException
    = HttpStatusCodeException
        (Int, SerializedByteString) SerializedByteString
    | HttpTooManyRedirects
    | HttpOverlongHeaders
    | HttpResponseTimeout
    | HttpConnectionTimeout
    | HttpConnectionFailure
    | HttpInvalidStatusLine SerializedByteString
    | HttpInvalidHeader SerializedByteString
    | HttpInvalidRequestHeader SerializedByteString
    | HttpInternalException
    | HttpProxyConnectException
        SerializedByteString Int (Int, SerializedByteString)
    | HttpNoResponseDataReceived
    | HttpTlsNotSupported
    | HttpWrongRequestBodyStreamSize Word64 Word64
    | HttpResponseBodyTooShort Word64 Word64
    | HttpInvalidChunkHeaders
    | HttpIncompleteHeaders
    | HttpInvalidDestinationHost SerializedByteString
    | HttpZlibException Int
    | HttpInvalidProxyEnvironmentVariable Text Text
    | HttpConnectionClosed
    | HttpInvalidProxySettings Text
    | HttpInvalidUrlException String String
    deriving (Show, Eq, Ord, Generic)

instance Aeson.ToJSON SerializedHttpException

instance Aeson.FromJSON SerializedHttpException



serializeStrippedRequest ::
    Servant.Client.Core.RequestF () (Servant.Client.Core.BaseUrl, BS.ByteString) ->
    (Text, SerializedRequest)
serializeStrippedRequest coreRequest =
    (baseUrl, serialized)
  where
    baseUrl =
        Text.pack $
        Servant.Client.Core.showBaseUrl $
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
                    SerializedByteString $
                    Network.HTTP.Media.renderHeader coreMediaType
    serialized =
        SerializedRequest
            { requestHttpVersion =
                case Servant.Client.Core.requestHttpVersion coreRequest of
                    Network.HTTP.Types.HttpVersion v1 v2 -> (v1, v2)
            , requestMethod =
                SerializedByteString $
                Servant.Client.Core.requestMethod coreRequest
            , requestPath =
                SerializedByteString $
                snd $
                Servant.Client.Core.requestPath coreRequest
            , requestQueryString =
                fmap
                    (\(name, mbValue) ->
                        ( SerializedByteString name
                        , fmap SerializedByteString mbValue
                        )
                    ) $
                Servant.Client.Core.requestQueryString coreRequest
            , requestAccept =
                fmap
                    (   SerializedByteString .
                        Network.HTTP.Media.renderHeader
                    ) $
                Servant.Client.Core.requestAccept coreRequest
            , requestHeaders =
                fmap
                    (\(name, value) ->
                        ( SerializedByteString $ CI.foldedCase name
                        , SerializedByteString value
                        )
                    ) $
                Servant.Client.Core.requestHeaders coreRequest
            , requestBody =
                mbSerialBody
            }

restoreStrippedRequest ::
    Text ->
    SerializedRequest ->
    Servant.Client.Core.RequestF () (Servant.Client.Core.BaseUrl, BS.ByteString)
restoreStrippedRequest baseUrlText serialRequest =
    Servant.Client.Core.Request
        { Servant.Client.Core.requestPath =
            ( baseUrl
            , getSerializedByteString $
                requestPath serialRequest
            )
        , Servant.Client.Core.requestQueryString =
                fmap
                    (\(name, mbValue) ->
                        ( getSerializedByteString name
                        , fmap getSerializedByteString mbValue
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
                    ( CI.mk $ getSerializedByteString name
                    , getSerializedByteString value
                    )
                ) $
            requestHeaders serialRequest
        , Servant.Client.Core.requestHttpVersion =
            case requestHttpVersion serialRequest of
                (v1, v2) -> Network.HTTP.Types.HttpVersion v1 v2
        , Servant.Client.Core.requestMethod =
            getSerializedByteString $
            requestMethod serialRequest
        }
  where
    !baseUrl =
        System.IO.Unsafe.unsafeDupablePerformIO $
        Servant.Client.Core.parseBaseUrl $
        Text.unpack baseUrlText
    !mbCoreBody =
        case requestBody serialRequest of
            Nothing -> Nothing
            Just (serialMediaType, _) -> Just ((), coreMT)
              where
                !coreMT = parseMediaType serialMediaType
    !accept =
        foldl'
            (\r x -> r :|> parseMediaType x)
            Empty $
        requestAccept serialRequest
    parseMediaType =
        maybe (error "bad accept") id .
        Network.HTTP.Media.parseAccept .
        getSerializedByteString

serializeRequest ::
    Servant.Client.Core.Request ->
    SerializedRequest
serializeRequest coreRequest =
    SerializedRequest
        { requestHttpVersion =
            case Servant.Client.Core.requestHttpVersion coreRequest of
                Network.HTTP.Types.HttpVersion v1 v2 -> (v1, v2)
        , requestMethod =
            SerializedByteString $
            Servant.Client.Core.requestMethod coreRequest
        , requestPath =
            SerializedByteString $
            BS.Lazy.toStrict $
            Binary.Builder.toLazyByteString $
            Servant.Client.Core.requestPath coreRequest
        , requestQueryString =
            fmap
                (\(name, mbValue) ->
                    ( SerializedByteString name
                    , fmap SerializedByteString mbValue
                    )
                ) $
            Servant.Client.Core.requestQueryString coreRequest
        , requestAccept =
            fmap
                (   SerializedByteString .
                    Network.HTTP.Media.renderHeader
                ) $
            Servant.Client.Core.requestAccept coreRequest
        , requestHeaders =
            fmap
                (\(name, value) ->
                    ( SerializedByteString $ CI.foldedCase name
                    , SerializedByteString value
                    )
                ) $
            Servant.Client.Core.requestHeaders coreRequest
        , requestBody =
            fmap
                (\(coreContents, coreMediaType) ->
                    ( SerializedByteString $
                        Network.HTTP.Media.renderHeader coreMediaType
                    , serializeRequestBody coreContents
                    )
                ) $
            Servant.Client.Core.requestBody coreRequest
        }

replayDrainRequest ::
    ReqId.ReqId ->
    Servant.Client.Core.Request ->
    Record.Replayer ()
replayDrainRequest reqId coreRequest =
    case Servant.Client.Core.requestBody coreRequest of
        Nothing -> pure ()
        Just (coreBody, _) ->
            replayDrainRequestBody reqId coreBody

wrapRecordingRequest ::
    Record.RecordTrace ->
    ReqId.ReqId ->
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
    Maybe SerializedByteString
serializeRequestBody coreBody =
    case coreBody of
        Servant.Client.Core.RequestBodyLBS lbs ->
            Just $ SerializedByteString $ BS.Lazy.toStrict lbs
        Servant.Client.Core.RequestBodyBS bs ->
            Just $ SerializedByteString bs
        Servant.Client.Core.RequestBodySource _ ->
            Nothing

wrapRecordingRequestBody ::
    Record.RecordTrace ->
    ReqId.ReqId ->
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
    ReqId.ReqId ->
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
    ReqId.ReqId ->
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
                    (SerializedByteString $ BS.Lazy.toStrict lbs)
                )
            pure $ Servant.Types.SourceT.Yield lbs (go next)
    go (Servant.Types.SourceT.Effect m) =
        Servant.Types.SourceT.Effect (go <$> m)

replayDrainSourceT ::
    ReqId.ReqId ->
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
                (SerializedByteString $ BS.Lazy.toStrict lbs)
            )
        go next
    go (Servant.Types.SourceT.Effect m) =
        Record.replayerLiftIO m >>= go



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
                    ( SerializedByteString $ CI.foldedCase name
                    , SerializedByteString value
                    )
                ) $
            Servant.Client.Core.responseHeaders coreResponse
        , responseHttpVersion =
            case Servant.Client.Core.responseHttpVersion coreResponse of
                Network.HTTP.Types.HttpVersion v1 v2 -> (v1, v2)
        , responseBody =
            Just $
                SerializedByteString $
                BS.Lazy.toStrict $
                Servant.Client.Core.responseBody coreResponse
        }

restoreResponse ::
    SerializedResponse ->
    (Maybe SerializedByteString -> a) ->
    Servant.Client.Core.ResponseF a
restoreResponse serialResponse handleBody =
    Servant.Client.Core.Response
        { Servant.Client.Core.responseStatusCode =
            restoreStatusCode $
            responseStatusCode serialResponse
        , Servant.Client.Core.responseHeaders =
            fmap
                (\(name, value) ->
                    ( CI.mk $ getSerializedByteString name
                    , getSerializedByteString value
                    )
                ) $
            responseHeaders serialResponse
        , Servant.Client.Core.responseHttpVersion =
            case responseHttpVersion serialResponse of
                (v1, v2) -> Network.HTTP.Types.HttpVersion v1 v2
        , Servant.Client.Core.responseBody =
            handleBody $ responseBody serialResponse
        }

makeSerializedStreamingResponse ::
    Record.RecordTrace ->
    ReqId.ReqId ->
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
                        ( SerializedByteString $ CI.foldedCase name
                        , SerializedByteString value
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
    ReqId.ReqId ->
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
    Maybe SerializedByteString ->
    BS.Lazy.ByteString
restoreStrippedResponseBody Nothing =
    ""
restoreStrippedResponseBody (Just sbs) =
    BS.Lazy.fromStrict $
    getSerializedByteString sbs

restoreResponseStreamingBody ::
    Record.ReplayContext ->
    ReqId.ReqId ->
    Maybe SerializedByteString ->
    Servant.Types.SourceT.SourceT IO BS.ByteString
restoreResponseStreamingBody rt reqId Nothing =
    replayRestoreResponseSourceT  rt reqId
restoreResponseStreamingBody _ _ (Just sbs) =
    Servant.Types.SourceT.fromStepT $
        Servant.Types.SourceT.Yield (getSerializedByteString sbs) $
        Servant.Types.SourceT.Stop

traceResponseSourceT ::
    Record.RecordTrace ->
    ReqId.ReqId ->
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
                (ResponseChunk (SerializedByteString bs))
            pure (go (i+1) next)
    go i (Servant.Types.SourceT.Effect m) =
        Servant.Types.SourceT.Effect (go i <$> m)

replayRestoreResponseSourceT ::
    Record.ReplayContext ->
    ReqId.ReqId ->
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
                            (getSerializedByteString sbs)
                            (go (i + 1))
                Right (ResponseError msg) ->
                    pure $ Servant.Types.SourceT.Error (Text.unpack msg)



serializeClientError ::
    Servant.Client.Core.ClientError ->
    SerializedClientError
serializeClientError (Servant.Client.Core.FailureResponse req resp) = do
    let (baseUrlText, serializedReq) =
            serializeStrippedRequest req
    let serializedResp =
            serializeResponse resp
    FailureResponse baseUrlText serializedReq serializedResp
serializeClientError (Servant.Client.Core.DecodeFailure msg resp) = do
    let serializedResp =
            serializeResponse resp
    DecodeFailure msg serializedResp
serializeClientError (Servant.Client.Core.UnsupportedContentType mt resp) = do
    let serializedMediaType =
            SerializedByteString $
            Network.HTTP.Media.renderHeader mt
    let serializedResp =
            serializeResponse resp
    UnsupportedContentType serializedMediaType serializedResp
serializeClientError (Servant.Client.Core.InvalidContentTypeHeader resp) = do
    let serializedResp =
            serializeResponse resp
    InvalidContentTypeHeader serializedResp
serializeClientError (Servant.Client.Core.ConnectionError ex)
    | Just httpException <- fromException ex =
        HttpException
            (serializeHttpException httpException)
    | otherwise =
        ConnectionError

restoreClientError ::
    SerializedClientError ->
    Servant.Client.Core.ClientError
restoreClientError (FailureResponse baseUrlText sreq sresp) = do
    let !req = restoreStrippedRequest baseUrlText sreq
    let !resp = restoreResponse sresp restoreStrippedResponseBody
    Servant.Client.Core.FailureResponse req resp
restoreClientError (DecodeFailure msg sresp) = do
    let !resp = restoreResponse sresp restoreStrippedResponseBody
    Servant.Client.Core.DecodeFailure msg resp
restoreClientError (UnsupportedContentType smt sresp) = do
    let !mediaType = parseMediaType smt
    let !resp = restoreResponse sresp restoreStrippedResponseBody
    Servant.Client.Core.UnsupportedContentType mediaType resp
  where
    parseMediaType =
        maybe (error "bad accept") id .
        Network.HTTP.Media.parseAccept .
        getSerializedByteString
restoreClientError (InvalidContentTypeHeader sresp) = do
    let !resp = restoreResponse sresp restoreStrippedResponseBody
    Servant.Client.Core.InvalidContentTypeHeader resp
restoreClientError (HttpException httpException) = do
    Servant.Client.Core.ConnectionError $
        toException $ restoreHttpException httpException
restoreClientError ConnectionError = do
    Servant.Client.Core.ConnectionError $ toException $ ErrorCall "connection error"

serializeHttpException ::
    Network.HTTP.Client.Internal.HttpException ->
    SerializedHttpException
serializeHttpException (Network.HTTP.Client.Internal.HttpExceptionRequest _ content) =
    case content of
        Network.HTTP.Client.Internal.StatusCodeException resp body ->
            HttpStatusCodeException
                (serializeStatusCode $ Network.HTTP.Client.Internal.responseStatus resp)
                (SerializedByteString body)
        Network.HTTP.Client.Internal.TooManyRedirects _ ->
            HttpTooManyRedirects
        Network.HTTP.Client.Internal.OverlongHeaders ->
            HttpOverlongHeaders
        Network.HTTP.Client.Internal.ResponseTimeout ->
            HttpResponseTimeout
        Network.HTTP.Client.Internal.ConnectionTimeout ->
            HttpConnectionTimeout
        Network.HTTP.Client.Internal.ConnectionFailure _ex ->
            HttpConnectionFailure
        Network.HTTP.Client.Internal.InvalidStatusLine line ->
            HttpInvalidStatusLine
                (SerializedByteString line)
        Network.HTTP.Client.Internal.InvalidHeader header ->
            HttpInvalidHeader
                (SerializedByteString header)
        Network.HTTP.Client.Internal.InvalidRequestHeader header ->
            HttpInvalidRequestHeader
                (SerializedByteString header)
        Network.HTTP.Client.Internal.InternalException _ ->
            HttpInternalException
        Network.HTTP.Client.Internal.ProxyConnectException host port status ->
            HttpProxyConnectException
                (SerializedByteString host)
                port
                (serializeStatusCode status)
        Network.HTTP.Client.Internal.NoResponseDataReceived ->
            HttpNoResponseDataReceived
        Network.HTTP.Client.Internal.TlsNotSupported ->
            HttpTlsNotSupported
        Network.HTTP.Client.Internal.WrongRequestBodyStreamSize expected actual ->
            HttpWrongRequestBodyStreamSize expected actual
        Network.HTTP.Client.Internal.ResponseBodyTooShort expected actual ->
            HttpResponseBodyTooShort expected actual
        Network.HTTP.Client.Internal.InvalidChunkHeaders ->
            HttpInvalidChunkHeaders
        Network.HTTP.Client.Internal.IncompleteHeaders ->
            HttpIncompleteHeaders
        Network.HTTP.Client.Internal.InvalidDestinationHost host ->
            HttpInvalidDestinationHost
                (SerializedByteString host)
        Network.HTTP.Client.Internal.HttpZlibException (Data.Streaming.Zlib.ZlibException code) ->
            HttpZlibException code
        Network.HTTP.Client.Internal.InvalidProxyEnvironmentVariable name value ->
            HttpInvalidProxyEnvironmentVariable name value
        Network.HTTP.Client.Internal.ConnectionClosed ->
            HttpConnectionClosed
        Network.HTTP.Client.Internal.InvalidProxySettings msg ->
            HttpInvalidProxySettings msg
serializeHttpException (Network.HTTP.Client.Internal.InvalidUrlException url msg) =
    HttpInvalidUrlException url msg

restoreHttpException ::
    SerializedHttpException ->
    Network.HTTP.Client.Internal.HttpException
restoreHttpException ex =
    case ex of
        HttpStatusCodeException status body ->
            exceptionRequest $
                Network.HTTP.Client.Internal.StatusCodeException
                    (defaultResponse $ restoreStatusCode status)
                    (getSerializedByteString body)
        HttpTooManyRedirects ->
            exceptionRequest $
                Network.HTTP.Client.Internal.TooManyRedirects []
        HttpOverlongHeaders ->
            exceptionRequest $
                Network.HTTP.Client.Internal.OverlongHeaders
        HttpResponseTimeout ->
            exceptionRequest $
                Network.HTTP.Client.Internal.ResponseTimeout
        HttpConnectionTimeout ->
            exceptionRequest $
                Network.HTTP.Client.Internal.ConnectionTimeout
        HttpConnectionFailure ->
            exceptionRequest $
                Network.HTTP.Client.Internal.ConnectionFailure
                    (toException $ ErrorCall "connection failure")
        HttpInvalidStatusLine line ->
            exceptionRequest $
                Network.HTTP.Client.Internal.InvalidStatusLine
                    (getSerializedByteString line)
        HttpInvalidHeader header ->
            exceptionRequest $
                Network.HTTP.Client.Internal.InvalidHeader
                    (getSerializedByteString header)
        HttpInvalidRequestHeader header ->
            exceptionRequest $
                Network.HTTP.Client.Internal.InvalidRequestHeader
                    (getSerializedByteString header)
        HttpInternalException ->
            exceptionRequest $
                Network.HTTP.Client.Internal.InternalException
                    (toException $ ErrorCall "internal exception")
        HttpProxyConnectException host port status ->
            exceptionRequest $
                Network.HTTP.Client.Internal.ProxyConnectException
                    (getSerializedByteString host)
                    port
                    (restoreStatusCode status)
        HttpNoResponseDataReceived ->
            exceptionRequest $
                Network.HTTP.Client.Internal.NoResponseDataReceived
        HttpTlsNotSupported ->
            exceptionRequest $
                Network.HTTP.Client.Internal.TlsNotSupported
        HttpWrongRequestBodyStreamSize expected actual ->
            exceptionRequest $
                Network.HTTP.Client.Internal.WrongRequestBodyStreamSize expected actual
        HttpResponseBodyTooShort expected actual ->
            exceptionRequest $
                Network.HTTP.Client.Internal.ResponseBodyTooShort expected actual
        HttpInvalidChunkHeaders ->
            exceptionRequest $
                Network.HTTP.Client.Internal.InvalidChunkHeaders
        HttpIncompleteHeaders ->
            exceptionRequest $
                Network.HTTP.Client.Internal.IncompleteHeaders
        HttpInvalidDestinationHost host ->
            exceptionRequest $
                Network.HTTP.Client.Internal.InvalidDestinationHost
                    (getSerializedByteString host)
        HttpZlibException code ->
            exceptionRequest $
                Network.HTTP.Client.Internal.HttpZlibException (Data.Streaming.Zlib.ZlibException code)
        HttpInvalidProxyEnvironmentVariable name value ->
            exceptionRequest $
                Network.HTTP.Client.Internal.InvalidProxyEnvironmentVariable name value
        HttpConnectionClosed ->
            exceptionRequest $
                Network.HTTP.Client.Internal.ConnectionClosed
        HttpInvalidProxySettings msg ->
            exceptionRequest $
                Network.HTTP.Client.Internal.InvalidProxySettings msg
        HttpInvalidUrlException url msg ->
            Network.HTTP.Client.Internal.InvalidUrlException url msg
  where
    exceptionRequest =
        Network.HTTP.Client.Internal.HttpExceptionRequest
            Network.HTTP.Client.Internal.defaultRequest
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
    (Int, SerializedByteString)
serializeStatusCode status =
    ( Network.HTTP.Types.statusCode status
    , SerializedByteString $
        Network.HTTP.Types.statusMessage status
    )

restoreStatusCode ::
    (Int, SerializedByteString) ->
    Network.HTTP.Types.Status
restoreStatusCode (sn, sb) =
    Network.HTTP.Types.mkStatus
        sn
        (getSerializedByteString sb)
