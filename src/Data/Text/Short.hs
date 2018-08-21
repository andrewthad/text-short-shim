module Data.Text.Short
    ( -- * The 'ShortText' type
      ShortText

      -- * Basic operations
      -- ** Construction
    , empty
    , singleton
    , pack
    , append
    , concat
    , cons
    , snoc
    , replicate

      -- ** Deconstruction
    , unpack
    , uncons
    , unsnoc

      -- ** Querying & predicates
    , null
    , length
    -- , isAscii
    , all
    , any
    , find
    , isPrefixOf
    , isSuffixOf

      -- ** Lookup & indexing
    -- , (!?)
    -- , indexMaybe
    -- , indexEndMaybe
    , findIndex

      -- * Splitting 'ShortText's
      -- ** Basic functions
    , take
    , takeEnd
    , drop
    , dropEnd
    , takeWhile
    , takeWhileEnd
    , dropWhile
    , dropWhileEnd

    , dropAround

      -- ** Pair-valued functions
    -- , splitAt
    -- , splitAtEnd
    , span
    -- , break
    -- , spanEnd
    -- , breakEnd

      -- ** Suffix & Prefix operations
    , stripPrefix
    , stripSuffix

      -- * Transformations
    , intersperse
    , intercalate
    , reverse
    , filter

      -- * Folds
    , foldl
    , foldl'
    , foldr

      -- * Conversions
      -- ** 'String'
    , fromString
    , toString

      -- ** 'Text'
    , fromText
    , toText

      -- ** 'ByteString'
    -- , fromShortByteString
    -- , toShortByteString

    -- , fromByteString
    -- , toByteString

    -- , toBuilder

    ) where

import Prelude hiding (reverse,filter,map,span,take,
  drop,foldl,foldr,concat,replicate,null,length,all,any,
  dropWhile,takeWhile)

import Data.Text

type ShortText = Text

toText :: ShortText -> Text
toText = id

fromText :: Text -> ShortText
fromText = id

toString :: ShortText -> String
toString = unpack

fromString :: String -> ShortText
fromString = pack

