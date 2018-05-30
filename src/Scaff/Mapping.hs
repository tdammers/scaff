{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE DeriveFunctor #-}
module Scaff.Mapping
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Text.Ginger
import Data.Aeson (Value (..))
import System.FilePath
import System.IO
import System.Directory
import Text.Printf
import Control.Monad.Writer
import Control.Exception
import Data.Char

import Scaff.Context
import Scaff.Ginger (gingerPure, gingerFileIO)

data MappingOf a =
  Mapping
    { template :: FilePath
    , path :: a
    }
    deriving (Functor)

type Mapping = MappingOf FilePath

type Mappings = [Mapping]

runMapping :: FilePath -> Context -> Mapping -> IO ()
runMapping templateRootDir context mapping = do
  let srcFn = templateRootDir </> template mapping
      dstFn = path mapping
  printf "%s\n" dstFn
  createDirectoryIfMissing True (takeDirectory dstFn)
  gingerFileIO context srcFn dstFn

parseMapping :: String -> Maybe Mapping
parseMapping input = do
  -- TODO: better parser
  let src = takeWhile (not . isSpace) . dropWhile isSpace . takeWhile (/= ':') $ input
      rem = drop 1 . dropWhile (/= ':') $ input
      dst = dropWhile isSpace $ rem
  if null dst then
    if null src then
      Nothing
    else
      Just (Mapping src src)
  else
    Just (Mapping src dst)

