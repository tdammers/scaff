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
import Scaff.Mapping
import Scaff.Ginger

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

parseArgs :: [String] -> IO (String, String, String, String)
parseArgs [fn, project, dstDir] = do
  isdir <- doesDirectoryExist fn
  let (templateDir, fn') =
        if isdir then
          (fn, fn </> "files")
        else
          (takeDirectory fn, fn)
  return (project, templateDir, fn', dstDir)
parseArgs [fn, project] = parseArgs [fn, project, "." </> project]
parseArgs _ =
  error "Usage: scaff TEMPLATE PROJECT-NAME"

main :: IO ()
main = do
  args <- getArgs
  (project, templateDir, mappingFn, dstDir) <- parseArgs args
  context <- getContext project
  mappings <- map (fmap (dstDir </>))
                . catMaybes
                . map parseMapping
                . lines
                . Text.unpack
                <$> (gingerPure context =<< readFile mappingFn)
  mapM_ (runMapping templateDir context) mappings
