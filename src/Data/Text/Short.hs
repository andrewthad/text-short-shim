{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Data.Text.Short
    ( -- * The 'ShortText' type
      ShortText(..)

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

    , fromByteString
    , toByteString

    -- , toBuilder

    ) where

import Prelude hiding (reverse,filter,map,span,take,
  drop,foldl,foldr,concat,replicate,null,length,all,any,
  dropWhile,takeWhile)

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

newtype ShortText = ShortText T.Text

toText :: ShortText -> T.Text
toText = coerce

fromText :: T.Text -> ShortText
fromText = coerce

toString :: ShortText -> String
toString = coerce T.unpack

fromString :: String -> ShortText
fromString = coerce T.pack

fromByteString :: ByteString -> Maybe ShortText
fromByteString = coerce (either (const Nothing) Just . TE.decodeUtf8')

toByteString :: ShortText -> ByteString
toByteString = coerce TE.encodeUtf8

foldr :: forall a. (Char -> a -> a) -> a -> ShortText -> a
foldr = coerce (T.foldr @a)

foldl :: forall a. (a -> Char -> a) -> a -> ShortText -> a
foldl = coerce (T.foldl @a)

foldl' :: forall a. (a -> Char -> a) -> a -> ShortText -> a
foldl' = coerce (T.foldl' @a)

filter :: (Char -> Bool) -> ShortText -> ShortText
filter = coerce T.filter

intercalate :: ShortText -> [ShortText] -> ShortText
intercalate = coerce T.intercalate

stripPrefix :: ShortText -> ShortText -> Maybe ShortText
stripPrefix = coerce T.stripPrefix

stripSuffix :: ShortText -> ShortText -> Maybe ShortText
stripSuffix = coerce T.stripSuffix

reverse :: ShortText -> ShortText
reverse = coerce T.reverse

dropAround :: (Char -> Bool) -> ShortText -> ShortText
dropAround = coerce T.dropAround

dropWhileEnd :: (Char -> Bool) -> ShortText -> ShortText
dropWhileEnd = coerce T.dropWhileEnd

span :: (Char -> Bool) -> ShortText -> (ShortText, ShortText)
span = coerce T.span

drop :: Int -> ShortText -> ShortText
drop = coerce T.drop

dropEnd :: Int -> ShortText -> ShortText
dropEnd = coerce T.dropEnd

take :: Int -> ShortText -> ShortText
take = coerce T.take

takeWhile :: (Char -> Bool) -> ShortText -> ShortText
takeWhile = coerce T.takeWhile

takeWhileEnd :: (Char -> Bool) -> ShortText -> ShortText
takeWhileEnd = coerce T.takeWhileEnd

isPrefixOf :: ShortText -> ShortText -> Bool
isPrefixOf = coerce T.isPrefixOf

isSuffixOf :: ShortText -> ShortText -> Bool
isSuffixOf = coerce T.isSuffixOf

findIndex :: (Char -> Bool) -> ShortText -> Maybe Int
findIndex = coerce T.findIndex

find :: (Char -> Bool) -> ShortText -> Maybe Char
find = coerce T.find

all :: (Char -> Bool) -> ShortText -> Bool
all = coerce T.all

any :: (Char -> Bool) -> ShortText -> Bool
any = coerce T.any

length :: ShortText -> Int
length = coerce T.length

null :: ShortText -> Bool
null = coerce T.null

empty :: ShortText
empty = coerce T.empty

singleton :: Char -> ShortText
singleton = coerce T.singleton

pack :: String -> ShortText
pack = coerce T.pack

append :: ShortText -> ShortText -> ShortText
append = coerce T.append

concat :: [ShortText] -> ShortText
concat = coerce T.concat

cons :: Char -> ShortText -> ShortText
cons = coerce T.cons

snoc :: ShortText -> Char -> ShortText
snoc = coerce T.snoc

replicate :: Int -> ShortText -> ShortText
replicate = coerce T.replicate

unpack :: ShortText -> [Char]
unpack = coerce T.unpack

dropWhile :: (Char -> Bool) -> ShortText -> ShortText
dropWhile = coerce T.dropWhile

takeEnd :: Int -> ShortText -> ShortText
takeEnd = coerce T.takeEnd

intersperse :: Char -> ShortText -> ShortText
intersperse = coerce T.intersperse

uncons :: ShortText -> Maybe (Char, ShortText)
uncons = coerce T.uncons

unsnoc :: ShortText -> Maybe (ShortText, Char)
unsnoc = coerce T.unsnoc

