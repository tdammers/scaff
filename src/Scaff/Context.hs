{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
module Scaff.Context
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

loadYamlFile :: JSON.FromJSON a => FilePath -> IO a
loadYamlFile fn =
  YAML.decodeFileEither fn >>= either (error . show) pure 

type Context = HashMap Text JSON.Value

getContext :: String -> IO Context
getContext project = do
  home <- getEnv "HOME"
  context0 :: HashMap Text JSON.Value <- loadYamlFile (home </> ".scaff/config.yaml")
  environment <- map (\(k, v) -> (Text.pack k, toJSON v)) <$> getEnvironment
  let environmentAll =
        HashMap.fromList environment
      environmentOurs =
        HashMap.fromList
          [ (cleanupEnvKey key, val)
          | (key, val) <- environment
          , "SCAFF_" `Text.isPrefixOf` key
          ]
      cleanupEnvKey :: Text -> Text
      cleanupEnvKey = Text.pack . kebab . Text.unpack . Text.drop 6
  let vars = [ ("project", toJSON project)
             , ("env", toJSON environmentAll)
             ]
  return $ context0  <> environmentOurs <> HashMap.fromList vars
