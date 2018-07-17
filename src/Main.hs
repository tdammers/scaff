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
import System.IO
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

askQuestions :: [Question] -> IO Context
askQuestions = fmap mconcat . mapM askQuestion

askQuestion :: Question -> IO Context
askQuestion (Question key question def) = do
  hPutStr stderr question
  hPutStr stderr " "
  hFlush stderr
  answer' <- getLine
  let answer = if answer' == "" then def else Text.pack answer'
  return $ contextSingleton key answer

main :: IO ()
main = do
  args <- getArgs
  (project, templateName, dstDir) <- parseArgs args
  config <- loadConfig
  template <- maybe (error $ "template not found: " ++ templateName) pure
               =<< findTemplate templateName (templateRepos config)
  questions <- readTemplateQuestions template
  extraVars <- askQuestions questions
  context <- getContext project (configVars config <> extraVars)
  mappings <- map (fmap (dstDir </>))
                . catMaybes
                . map parseMapping
                . lines
                . Text.unpack
                <$> ( gingerPure context
                      =<<
                      readTemplateFileOrElse
                        template
                        (error "Could not find template files listing")
                        "files"
                    )
  mapM_ (runMapping template context) mappings
