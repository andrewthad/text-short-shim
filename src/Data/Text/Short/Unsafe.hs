module Data.Text.Short.Unsafe
  ( fromShortByteStringUnsafe
  ) where

import Data.Text.Short (ShortText)
import Data.ByteString.Short (ShortByteString)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Short as SB

fromShortByteStringUnsafe :: ShortByteString -> ShortText
fromShortByteStringUnsafe = TE.decodeUtf8 . SB.fromShort
