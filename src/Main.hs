{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
module Main
where

import Control.Monad
import Control.Applicative
import qualified Data.Yaml as YAML
import qualified Data.Aeson as JSON
import Data.Aeson (toJSON)
import System.FilePath
import Scaff.Mapping (Mapping, runMapping)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import System.Environment
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Char
import Data.Maybe
import Data.Bool
import System.Directory
import Data.Monoid

import Scaff.Mapping

loadYaml :: JSON.FromJSON a => IO a
loadYaml =
  YAML.decodeEither' <$> BS.getContents >>= either (error . show) pure 

loadYamlFile :: JSON.FromJSON a => FilePath -> IO a
loadYamlFile fn =
  YAML.decodeFileEither fn >>= either (error . show) pure 

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
  home <- getEnv "HOME"
  context0 :: HashMap Text JSON.Value <- loadYamlFile (home </> ".scaff/config.yaml")
  environment <- HashMap.fromList . map (fmap toJSON) <$> getEnvironment
  args <- getArgs
  (project, templateDir, mappingFn, dstDir) <- parseArgs args
  let vars = [ ("project", toJSON project)
             , ("env", toJSON environment)
             ]
  let context = context0 <> HashMap.fromList vars
  mappings <- map (fmap (dstDir </>)) . catMaybes . map parseMapping . lines <$> readFile mappingFn
  mapM_ (runMapping templateDir context) mappings
