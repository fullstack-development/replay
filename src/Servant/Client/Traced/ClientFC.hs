{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Servant.Client.Traced.ClientFC
where

import Control.Monad.Except
import Data.Proxy
import qualified Network.HTTP.Types
import qualified Servant.Client.Core
import qualified Servant.Client.Streaming

type Client = ClientFC 'NoStreaming

type StreamingClient = ClientFC 'Streaming

client ::
    forall api.
    (Servant.Client.Core.HasClient Client api) =>
    Proxy api ->
    Servant.Client.Core.Client Client api
client p =
    Servant.Client.Core.clientIn
        p
        (Proxy @Client)

streamingClient ::
    forall api.
    (Servant.Client.Core.HasClient StreamingClient api) =>
    Proxy api ->
    Servant.Client.Core.Client StreamingClient api
streamingClient p =
    Servant.Client.Core.clientIn
        p
        (Proxy @StreamingClient)

data IsStreaming = Streaming | NoStreaming

type ClientFCReqHandler q =
    Maybe [Network.HTTP.Types.Status] ->
    Servant.Client.Core.Request ->
    (Servant.Client.Core.ClientError -> q) ->
    (Servant.Client.Core.Response -> q) ->
    q

type ClientFCWithSReqHandler q =
    forall t.
    Servant.Client.Core.Request ->
    (Servant.Client.Streaming.StreamingResponse -> IO t) ->
    (Servant.Client.Core.ClientError -> q) ->
    (t -> q) ->
    q

newtype ClientFC (s :: IsStreaming) a = ClientFC
    { runClientFC ::
        forall q.
        (Servant.Client.Core.ClientError -> q) ->
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

instance MonadIO (ClientFC s) where
    liftIO io =
        ClientFC $ \_onThrow _onReq _onWithSReq onIO cont ->
            onIO (cont <$> io)

instance MonadError Servant.Client.Core.ClientError (ClientFC s) where
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
