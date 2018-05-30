{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
module Scaff.Config
where

import qualified Data.Aeson as JSON
import qualified Data.Yaml as YAML
import Data.Aeson (toJSON)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import System.Environment
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid
import System.FilePath
import System.Directory
import Text.Casing
import Data.Maybe
import System.Directory
import System.FilePath
import System.IO
import Control.Exception
import System.IO.Error
import Data.Char
import Data.List

import Scaff.Context

loadYamlFile :: JSON.FromJSON a => FilePath -> IO a
loadYamlFile fn =
  YAML.decodeFileEither fn >>= either (error . show) pure 

loadOptionalYamlFile :: JSON.FromJSON a => a -> FilePath -> IO a
loadOptionalYamlFile def fn = do
  exists <- doesFileExist fn
  if exists then
    loadYamlFile fn
  else
    return def

data TemplateRepo
  = LocalRepo FilePath

data Config
  = Config
      { configVars :: Context
      , templateRepos :: [TemplateRepo]
      }

collectRepos :: FilePath -> IO [TemplateRepo]
collectRepos fn = do
  exists <- doesFileExist fn
  if exists then
    catMaybes . map parseRepoLine . lines <$> readFile fn
  else do
    return []

parseRepoLine :: String -> Maybe TemplateRepo
parseRepoLine str =
  case tyName of
    "local" -> Just (LocalRepo arg)
    x -> Nothing
  where
    strWithoutComments = takeWhile (/= '#') str
    tyName = takeWhile (not . isSpace) strWithoutComments
    arg = dropWhileEnd isSpace
            . dropWhile isSpace
            . dropWhile (not . isSpace)
            $ strWithoutComments

loadConfig :: IO Config
loadConfig = do
  home <- getEnv "HOME"
  let yamlFiles =
        [ "/etc/scaff/config.yaml"
        , "/usr/local/etc/scaff/config.yaml"
        , home </> ".scaff/config.yaml"
        ] :: [FilePath]
  vars <- mconcat <$> mapM (loadOptionalYamlFile HashMap.empty) yamlFiles

  repoLists <- do
    let mainRepoFile = home </> ".scaff/repos.list"
    otherRepoFiles <-
      (listDirectory $ home </> ".scaff/repos.list.d")
      `catch`
      (\err -> if isDoesNotExistError err then
                 return []
               else
                 throw err
      )
    return $ mainRepoFile:otherRepoFiles

  repos <- ((LocalRepo ".") :) . concat <$> mapM collectRepos repoLists

  return Config
    { configVars = vars
    , templateRepos = repos
    }

findTemplate :: String -> [TemplateRepo] -> IO (Maybe FilePath)
findTemplate templateName [] = pure Nothing
findTemplate templateName (repo:repos) = do
  foundMay <- findTemplateIn templateName repo
  case foundMay of
    Nothing -> findTemplate templateName repos
    x -> pure x

findTemplateIn :: String -> TemplateRepo -> IO (Maybe FilePath)
findTemplateIn templateName (LocalRepo dirname) = do
  let templateDir = dirname </> templateName
      fullPath = templateDir </> "files"
  exists <- doesFileExist fullPath
  if exists then
    pure . Just $ templateDir
  else
    pure Nothing

