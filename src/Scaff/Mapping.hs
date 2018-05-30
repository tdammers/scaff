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
