{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
module Main
where

import Control.Monad
import Control.Applicative
import System.FilePath
import System.Environment
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Char
import Data.Maybe
import Data.Bool
import System.Directory
import Data.Monoid
import Text.Casing
import Data.List

import Scaff.Context
import Scaff.Config
import Scaff.TemplateRepo
import Scaff.Mapping
import Scaff.Ginger

parseArgs :: [String] -> IO (String, String, String)
parseArgs [fn, project, dstDir] = do
  return (project, fn, dstDir)
parseArgs [fn, project] =
  parseArgs [fn, project, "." </> project]
parseArgs _ =
  error "Usage: scaff TEMPLATE PROJECT-NAME"

main :: IO ()
main = do
  args <- getArgs
  (project, templateName, dstDir) <- parseArgs args
  config <- loadConfig
  templateDir <- maybe (error $ "template not found: " ++ templateName) pure
                  =<< findTemplate templateName (templateRepos config)
  context <- getContext project (configVars config)
  let mappingFn = templateDir </> "files"
  mappings <- map (fmap (dstDir </>))
                . catMaybes
                . map parseMapping
                . lines
                . Text.unpack
                <$> (gingerPure context =<< readFile mappingFn)
  mapM_ (runMapping templateDir context) mappings
