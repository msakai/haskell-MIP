{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.MIP.FileUtils
-- Copyright   :  (c) Masahiro Sakai 2018
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Numeric.Optimization.MIP.FileUtils
  ( ParseError
  , getBaseExtension
  , isGZipSupported
  , readTextFile
  , writeTextFile
  ) where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import Data.Char (toLower)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLEncoding
import qualified Data.Text.Lazy.IO as TLIO
import Data.Void
import GHC.IO.Encoding (getLocaleEncoding)
import System.FilePath (takeExtension, splitExtension)
import System.IO
import qualified Text.Megaparsec as MP

import qualified Numeric.Optimization.MIP.Base as MIP

#ifdef WITH_ZLIB
import qualified Codec.Compression.GZip as GZip
import Data.ByteString.Lazy.Encoding (encode, decode)
#endif

-- | Error type for parsing.
--
-- The definition is slightly different based on the @megaparsec@ version.
type ParseError s = MP.ParseErrorBundle s Void

-- | Get base extension of a filename.
--
-- Supported compression format extensions (e.g. @.gz@) are removed, and extensions such as @.cnf@ are returned.
--
-- @since 0.2.1.0
getBaseExtension :: String -> String
getBaseExtension fname | (base, ext) <- splitExtension fname =
  case map toLower ext of
#ifdef WITH_ZLIB
    ".gz" -> getBaseExtension base
#endif
    s -> s

-- | Whether this library is build with gzip support.
--
-- @since 0.2.1.0
isGZipSupported :: Bool
#ifdef WITH_ZLIB
isGZipSupported = True
#else
isGZipSupported = False
#endif

-- | Read a (possibly compressed) text file.
--
-- If the library is build with gzip support ('isGZipSupported'), @.gz@ files are automatically decompressed.
--
-- @since 0.2.1.0
readTextFile :: MIP.FileOptions -> FilePath -> IO TL.Text
readTextFile opt fname = do
  enc <- case MIP.optFileEncoding opt of
           Nothing -> getLocaleEncoding
           Just enc -> return enc

  if CI.mk (show enc) `elem` ["UTF-8", "ASCII", "US-ASCII"] then do
#ifdef WITH_ZLIB
    let f = if CI.mk (takeExtension fname) == ".gz" then GZip.decompress else id
#else
    let f = id
#endif
    s <- f <$> BL.readFile fname
    return $ TLEncoding.decodeUtf8 s
#ifdef WITH_ZLIB
  else if CI.mk (takeExtension fname) == ".gz" then do
    s <- GZip.decompress <$> BL.readFile fname
    return $ decode enc s
#endif
  else do
    h <- openFile fname ReadMode
    hSetEncoding h enc
    TLIO.hGetContents h

-- | Read a (possibly compressed) text file.
--
-- If the library is build with gzip support ('isGZipSupported'), @.gz@ files are automatically compressed.
--
-- @since 0.2.1.0
writeTextFile
  :: MIP.FileOptions
  -> FilePath
  -> Bool -- ^ specify @True@ if the input is guaranteed to be a subset of ASCII characters.
  -> TL.Text
  -> IO ()
writeTextFile opt fname asciiSafe s = do
  enc <- case MIP.optFileEncoding opt of
           Nothing -> getLocaleEncoding
           Just enc -> return enc
  let encName = CI.mk (show enc)

#ifdef WITH_ZLIB
  if CI.mk (takeExtension fname) == ".gz" then do
    let bs =
          if encName == "UTF-8" || asciiSafe && (encName `elem` ["ASCII", "US-ASCII"]) then
            TLEncoding.encodeUtf8 s
          else
            encode enc s
    BL.writeFile fname $ GZip.compress bs
#else
  if False then do
    undefined
#endif
  else if encName == "UTF-8" || asciiSafe && (encName `elem` ["ASCII", "US-ASCII"]) then do
#if MIN_VERSION_bytestring(0,11,2)
    BB.writeFile fname $ TLEncoding.encodeUtf8Builder s
#else
    withBinaryFile fname WriteMode $ \h -> do
      BB.hPutBuilder h (TLEncoding.encodeUtf8Builder s)
#endif
  else do
    withFile fname WriteMode $ \h -> do
      hSetNewlineMode h noNewlineTranslation
      hSetEncoding h enc
      TLIO.hPutStr h s
