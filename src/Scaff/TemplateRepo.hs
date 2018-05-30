{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
module Scaff.TemplateRepo
where

import System.Environment
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid
import Data.Maybe
import System.Directory
import System.FilePath
import System.IO
import Control.Exception
import System.IO.Error
import Data.Char
import Data.List

data TemplateRepo
  = LocalRepo FilePath

listRepos :: IO [TemplateRepo]
listRepos = do
  home <- getEnv "HOME"
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

  ((LocalRepo ".") :) . concat <$> mapM collectRepos repoLists

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
