module Servant.Client.Traced.RandomId
    (
      generateRandomId
    )
where

import Data.IORef
import qualified Codec.Base16
import qualified Crypto.Random as Random
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified System.IO.Unsafe

generatorStateIORef :: IORef Random.ChaChaDRG
generatorStateIORef = System.IO.Unsafe.unsafePerformIO $ do
    state <- Random.drgNew
    newIORef state
{-# NOINLINE generatorStateIORef #-}

generateBytes :: Int -> IO BS.ByteString
generateBytes n = do
    atomicModifyIORef' generatorStateIORef $ \s1 ->
        case Random.randomBytesGenerate n s1 of
            (r, s2) -> (s2, r)

generateHexText :: Int -> IO Text.Text
generateHexText n =
    Codec.Base16.encode <$> generateBytes n

generateRandomId :: IO Text.Text
generateRandomId = generateHexText 8
