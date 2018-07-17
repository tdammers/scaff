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
import Data.Maybe

type Context = HashMap Text JSON.Value

contextLookup :: Text -> Context -> JSON.Value
contextLookup key context = fromMaybe JSON.Null $ HashMap.lookup key context

contextSingleton :: JSON.ToJSON a => Text -> a -> Context
contextSingleton k v = HashMap.singleton k (JSON.toJSON v)

getContext :: String -> Context -> IO Context
getContext project config = do
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
             , ("author", contextLookup "author" config)
             ]
  return $ environmentOurs <> HashMap.fromList vars
