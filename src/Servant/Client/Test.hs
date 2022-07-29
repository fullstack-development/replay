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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Client.Test
    -- (
      -- Reducible (..)
    -- , applyLifting
    -- , Lifting (..)
    -- , MonadLifting (..)
    -- )
where

import Control.Applicative
import Control.Concurrent
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
import Data.Void
import GHC.Generics
import qualified Type.Reflection as Type
import qualified Control.Concurrent.Async as Async
import qualified Control.Monad.Catch.Pure as Catch
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
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.IO as Text.IO
import qualified Data.Yaml.Pretty as Yaml.Pretty
import qualified Data.Yaml.TH as Yaml.TH
import qualified Network.HTTP.Client
import qualified Network.HTTP.Media
import qualified Network.HTTP.Types
import qualified Network.Wai
import qualified Network.Wai.Handler.Warp
import qualified Network.WebSockets
import qualified Servant
import qualified Servant.API.WebSocket
import qualified Servant.Client
import qualified Servant.Client.Core
import qualified Servant.Client.Core.RunClient
import qualified Servant.Client.Streaming
import qualified Servant.Client.Traced as Traced
import qualified Servant.Client.Traced.SerializedByteString as SBS
import qualified Servant.Client.Traced.RandomId as RandomId
import qualified Servant.Types.SourceT as SourceT
import qualified System.IO.Unsafe

iomvar :: MVar ()
iomvar = System.IO.Unsafe.unsafePerformIO $ newMVar ()
{-# NOINLINE iomvar #-}

writeLn :: (MonadIO m) => String -> m ()
writeLn str =
    -- pure ()
    liftIO $ withMVar iomvar $ \() -> putStrLn str

type OnceAPI =
    "once" Servant.:>
    Servant.ReqBody '[Servant.OctetStream] BS.ByteString Servant.:>
    Servant.Post '[Servant.PlainText] Text.Text

type StreamAPI =
    "stream" Servant.:>
    Servant.StreamBody Servant.NoFraming Servant.OctetStream (StreamT BS.ByteString) Servant.:>
    Servant.StreamPost Servant.NoFraming Servant.PlainText (StreamT Text.Text)

type API = OnceAPI Servant.:<|> StreamAPI

type StreamT = SourceT.SourceT IO

serveOnce :: BS.ByteString -> Servant.Handler Text.Text
serveOnce bs = do
    pure (SBS.encode bs)

serveStream ::
    StreamT BS.ByteString ->
    Servant.Handler (StreamT Text.Text)
serveStream bsSource = do
    liftIO $ writeLn "Server: serveStream"
    pure $ SourceT.mapStepT doStep bsSource
  where
    doStep SourceT.Stop =
        SourceT.Effect $ do
            writeLn "Server: Stop"
            pure $ SourceT.Stop
    doStep (SourceT.Error msg) =
        SourceT.Effect $ do
            writeLn $ "Server: Error " <> show msg
            pure $ SourceT.Error msg
    doStep (SourceT.Skip next) =
        SourceT.Effect $ do
            writeLn "Server: Skip"
            pure $ SourceT.Skip (doStep next)
    doStep (SourceT.Yield bs next) =
        SourceT.Effect $ do
            writeLn $ "Server: Yield " <> show (BS.take 40 bs)
            pure $ SourceT.Yield (SBS.encode bs) (doStep next)
    doStep (SourceT.Effect cont) =
        SourceT.Effect $ do
            writeLn "Server: Effect"
            doStep <$> cont

app :: Servant.Application
app =
    Servant.serve (Proxy @API) $
        serveOnce Servant.:<|> serveStream

clientOnce :: BS.ByteString -> Traced.Client Text.Text
clientOnce = Traced.client (Proxy @OnceAPI)

clientStream ::
    SourceT.SourceT IO BS.ByteString ->
    Traced.StreamingClient (SourceT.SourceT IO Text.Text)
clientStream = Traced.streamingClient (Proxy @StreamAPI)

doClient :: IO ()
doClient = do
    threadDelay 1000000
    httpManager <-
        Network.HTTP.Client.newManager
            Network.HTTP.Client.defaultManagerSettings
    let clientEnv =
            Servant.Client.Streaming.mkClientEnv
                httpManager
                (Servant.Client.BaseUrl
                    Servant.Client.Http
                    "localhost"
                    19877
                    ""
                )
    let bsStep bss =
            SourceT.Effect $ do
                threadDelay 1000000
                case bss of
                    [] ->
                        pure $ SourceT.Stop
                    a : rest ->
                        pure $ SourceT.Yield a (bsStep rest)
    let bsSource =
            SourceT.fromStepT $
                bsStep
                    [ "asdf"
                    , "1234"
                    , "~~"
                    ]
    flip runReaderT clientEnv $ do
        flip runReaderT () $ do
            Record.recording $ do
                Traced.withClientM (clientStream bsSource) $ \et -> do
                    case et of
                        Left err -> do
                            writeLn $ "Client: Initial error: " <> show err
                        Right textSource -> do
                            writeLn $ "Client: stream"
                            liftIO $ SourceT.unSourceT textSource doTextStep
    pure ()
  where
    doTextStep SourceT.Stop = do
        writeLn "Client: Stop"
    doTextStep (SourceT.Error msg) = do
        writeLn $ "Client: Error " <> show msg
    doTextStep (SourceT.Skip next) = do
        writeLn "Client: Skip"
        doTextStep next
    doTextStep (SourceT.Yield tx next) = do
        writeLn $ "Client: Yield " <> show (Text.take 40 tx)
        doTextStep next
    doTextStep (SourceT.Effect cont) = do
        writeLn "Client: Effect"
        doTextStep =<< cont

-- doClient :: IO ()
-- doClient = do
    -- threadDelay 1000000
    -- httpManager <-
        -- Network.HTTP.Client.newManager
            -- Network.HTTP.Client.defaultManagerSettings
    -- let clientEnv =
            -- Servant.Client.Streaming.mkClientEnv
                -- httpManager
                -- (Servant.Client.BaseUrl
                    -- Servant.Client.Http
                    -- "localhost"
                    -- 19877
                    -- ""
                -- )
    -- Record.recording $ Record.RecordT $ \rt -> do
        -- flip runReaderT (rt, clientEnv) $ do
            -- et <- Traced.runClientM (clientOnce "as~df")
            -- writeLn $ "Client: " <> show et
    -- pure ()

main :: IO ()
main =
    Async.race_
        (Network.Wai.Handler.Warp.run 19877 app)
        doClient



testReplay :: IO ()
testReplay = do
    rt <- Record.createReplayContext preparedTestTrace
    er <- flip Record.runReplayer rt $ do
        let bsStep bss =
                SourceT.Effect $ do
                    -- threadDelay 1000000
                    case bss of
                        [] ->
                            pure $ SourceT.Stop
                        a : rest ->
                            pure $ SourceT.Yield a (bsStep rest)
        let bsSource =
                SourceT.fromStepT $
                    bsStep
                        [ "asdf"
                        , "1234"
                        , "~~"
                        ]
        Traced.withClientM (clientStream bsSource) $ \et -> do
            case et of
                Left err -> do
                    Record.replayerLiftIO $ writeLn $ "Client: Initial error: " <> show err
                Right textSource -> do
                    Record.replayerLiftIO $ writeLn $ "Client: stream"
                    Record.replayerLiftIO $ SourceT.unSourceT textSource doTextStep
        pure ()
    effects <- atomically $ readTVar (Record.replayContextEffectsTVar rt)
    BS.Char8.putStrLn $ Record.renderYaml effects
    print er
  where
    doTextStep SourceT.Stop = do
        writeLn "Client: Stop"
    doTextStep (SourceT.Error msg) = do
        writeLn $ "Client: Error " <> show msg
    doTextStep (SourceT.Skip next) = do
        writeLn "Client: Skip"
        doTextStep next
    doTextStep (SourceT.Yield tx next) = do
        writeLn $ "Client: Yield " <> show (Text.take 40 tx)
        doTextStep next
    doTextStep (SourceT.Effect cont) = do
        writeLn "Client: Effect"
        doTextStep =<< cont



unJSON :: (Aeson.FromJSON a) => Aeson.Value -> a
unJSON j = case Aeson.fromJSON j of
    Aeson.Success x -> x
    Aeson.Error e -> error e

testTrace :: Seq (Record.TextTagged Record.TraceEvent)
testTrace = unJSON $ [Yaml.TH.yamlQQ|
- MonadServantClient:
    request:
    - start:
        requestAccept:
        - text/plain;charset=utf-8
        requestBody:
        - application/octet-stream
        - null
        requestHeaders: []
        requestHttpVersion:
        - 1
        - 1
        requestMethod: POST
        requestPath: /stream
        requestQueryString: []
    - success: 79dfc4116b7ea22d
- MonadServantClient:
    syncPoint:
      requestAccept:
      - text/plain;charset=utf-8
      requestBody:
      - application/octet-stream
      - null
      requestHeaders: []
      requestHttpVersion:
      - 1
      - 1
      requestMethod: POST
      requestPath: /stream
      requestQueryString: []
- MonadServantClient:
    effect:
      request:
      - 79dfc4116b7ea22d
      - requestAccept:
        - text/plain;charset=utf-8
        requestBody:
        - application/octet-stream
        - null
        requestHeaders: []
        requestHttpVersion:
        - 1
        - 1
        requestMethod: POST
        requestPath: /stream
        requestQueryString: []
- MonadServantClient:
    effect:
      requestChunk:
      - 79dfc4116b7ea22d
      - asdf
- MonadServantClient:
    effect:
      requestChunk:
      - 79dfc4116b7ea22d
      - '1234'
- MonadServantClient:
    effect:
      requestChunk:
      - 79dfc4116b7ea22d
      - ~7E~7E
- MonadServantClient:
    request:
    - outcome: 79dfc4116b7ea22d
    - success:
        contents:
          responseBody: null
          responseHeaders:
          - - transfer-encoding
            - chunked
          - - date
            - Thu, 28 Jul 2022 17:21:01 GMT
          - - server
            - Warp/3.3.17
          - - content-type
            - text/plain;charset=utf-8
          responseHttpVersion:
          - 1
          - 1
          responseStatusCode:
          - 200
          - OK
        tag: RequestOutcomeResponse
- MonadServantClient:
    request:
    - responseChunk:
      - 79dfc4116b7ea22d
      - 0
    - success:
        contents: asdf
        tag: ResponseChunk
- MonadServantClient:
    request:
    - responseChunk:
      - 79dfc4116b7ea22d
      - 1
    - success:
        contents: '1234'
        tag: ResponseChunk
- MonadServantClient:
    request:
    - responseChunk:
      - 79dfc4116b7ea22d
      - 2
    - success:
        contents: ~7E7E~7E7E
        tag: ResponseChunk
- MonadServantClient:
    request:
    - responseChunk:
      - 79dfc4116b7ea22d
      - 3
    - success:
        tag: ResponseEnd
  |]

preparedTestTrace :: Record.PreparedTrace
preparedTestTrace = Record.prepareTrace testTrace
