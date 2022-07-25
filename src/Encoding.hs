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

module Encoding
    (
      encode
    , decode
    )
where

import Data.Bits
import Data.Word
import Data.Char
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Text as Text

newtype SerializedByteString = SerializedByteString
    { getSerializedByteString :: BS.ByteString
    }
    deriving newtype (Show, Eq, Ord)

instance Aeson.ToJSON SerializedByteString where
    toJSON = Aeson.toJSON . encode . getSerializedByteString
    toEncoding = Aeson.toEncoding . encode . getSerializedByteString

instance Aeson.FromJSON SerializedByteString where
    parseJSON v = SerializedByteString . decode <$> Aeson.parseJSON v

encode :: BS.ByteString -> Text.Text
encode = Text.pack . encodeList . BS.unpack

decode :: Text.Text -> BS.ByteString
decode = BS.pack . decodeList . Text.unpack

encodeList :: [Word8] -> [Char]
encodeList (0x7e : rest) =
    0x7e %: encodeList rest
encodeList (b : rest)
    | b >= 0x20 && b <= 0x7e =
        chr (fromIntegral b) : encodeList rest
encodeList (b1 : b2 : rest)
    | b1 >= 0xc0 && b1 <= 0xdf
    , b2 >= 0x80 && b2 <= 0xbf
    , ccode >= 0x80 =
        chr ccode : encodeList rest
  where
    ccode =
        fromIntegral b1 `shiftL` 6 +
        fromIntegral b2 - 0x3080
encodeList (b1 : b2 : b3 : rest)
    | b1 >= 0xe0 && b1 <= 0xef
    , b2 >= 0x80 && b2 <= 0xbf
    , b3 >= 0x80 && b3 <= 0xbf
    , (ccode >= 0x800 && ccode <= 0xd7ff) || (ccode >= 0xe000) =
        chr ccode : encodeList rest
  where
    ccode =
        fromIntegral b1 `shiftL` 12 +
        fromIntegral b2 `shiftL` 6 +
        fromIntegral b3 - 0xe2080
encodeList (b1 : b2 : b3 : b4 : rest)
    | b1 >= 0xf0 && b1 <= 0xf7
    , b2 >= 0x80 && b2 <= 0xbf
    , b3 >= 0x80 && b3 <= 0xbf
    , b4 >= 0x80 && b4 <= 0xbf
    , ccode >= 0x10000 =
        chr ccode : encodeList rest
  where
    ccode =
        fromIntegral b1 `shiftL` 18 +
        fromIntegral b2 `shiftL` 12 +
        fromIntegral b3 `shiftL` 6 +
        fromIntegral b4 - 0x3c82080
encodeList (b : rest) =
    b %: encodeList rest
encodeList [] =
    []

infixr 5 %:
(%:) :: Word8 -> [Char] -> [Char]
b %: rest =
    '~' : hex (b `shiftR` 4) : hex (b .&. 15) : rest

decodeList :: [Char] -> [Word8]
decodeList ('~' : c1 : c2 : rest)
    | Just w1 <- fromHex c1
    , Just w2 <- fromHex c2 =
        (w1 `shiftL` 4 + w2) :
        decodeList rest
decodeList (c : rest)
    | i <= 0x7f =
        fromIntegral i :
        decodeList rest
    | i <= 0x7ff =
        fromIntegral (i `shiftR` 6 + 0xc0) :
        fromIntegral (i .&. 0x3f + 0x80) :
        decodeList rest
    | i <= 0xffff =
        fromIntegral (i `shiftR` 12 + 0xe0) :
        fromIntegral ((i `shiftR` 6) .&. 0x3f + 0x80) :
        fromIntegral (i .&. 0x3f + 0x80) :
        decodeList rest
    | otherwise =
        fromIntegral (i `shiftR` 18 + 0xf0) :
        fromIntegral ((i `shiftR` 12) .&. 0x3f + 0x80) :
        fromIntegral ((i `shiftR` 6) .&. 0x3f + 0x80) :
        fromIntegral (i .&. 0x3f + 0x80) :
        decodeList rest
  where
    i = ord c
decodeList [] =
    []

hex :: Word8 -> Char
hex w
    | w < 10 = chr (fromIntegral w + ord '0')
    | otherwise = chr (fromIntegral w + (ord 'A' - 10))

fromHex :: Char -> Maybe Word8
fromHex c
    | '0' <= c && c <= '9' = Just (fromIntegral (ord c - ord '0'))
    | 'A' <= c && c <= 'F' = Just (fromIntegral (ord c - (ord 'A' - 10)))
    | 'a' <= c && c <= 'f' = Just (fromIntegral (ord c - (ord 'a' - 10)))
    | otherwise = Nothing
