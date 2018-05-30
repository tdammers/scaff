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
import Scaff.Mapping
import Scaff.TemplateRepo

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

data Config
  = Config
      { configVars :: Context
      , templateRepos :: [TemplateRepo]
      }

loadConfig :: IO Config
loadConfig = do
  home <- getEnv "HOME"
  let yamlFiles =
        [ "/etc/scaff/config.yaml"
        , "/usr/local/etc/scaff/config.yaml"
        , home </> ".scaff/config.yaml"
        ] :: [FilePath]
  vars <- mconcat <$> mapM (loadOptionalYamlFile HashMap.empty) yamlFiles

  repos <- listRepos

  return Config
    { configVars = vars
    , templateRepos = repos
    }
