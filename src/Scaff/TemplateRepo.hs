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

data RepoRef
  = LocalRepo FilePath

listRepos :: IO [RepoRef]
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

collectRepos :: FilePath -> IO [RepoRef]
collectRepos fn = do
  exists <- doesFileExist fn
  if exists then
    fmap catMaybes . mapM parseRepoLine
    =<<
    (lines <$> readFile fn)
  else do
    return []

parseRepoLine :: String -> IO (Maybe RepoRef)
parseRepoLine str =
  case tyName of
    "local" ->
      return $ Just (LocalRepo arg)
    "" ->
      return Nothing
    x -> do
      hPutStrLn stderr $ "Warning: unrecognized repository type '" ++ tyName ++ "'"
      return Nothing
  where
    strWithoutComments = takeWhile (/= '#') . dropWhile isSpace $ str
    tyName = takeWhile (not . isSpace) strWithoutComments
    arg = dropWhileEnd isSpace
            . dropWhile isSpace
            . dropWhile (not . isSpace)
            $ strWithoutComments

data Template
  =  Template
      { readTemplateFile :: FilePath -> IO (Maybe String)
      }

readTemplateFileOrElse :: Template -> IO () -> FilePath -> IO String
readTemplateFileOrElse tpl barf fn =
  readTemplateFile tpl fn >>= maybe (barf >> return "") return

fsTemplate :: FilePath -> Template
fsTemplate templateDir =
  Template
    { readTemplateFile = \fn -> do
        let fullPath = templateDir </> fn
        exists <- doesFileExist fullPath
        if exists then
          Just <$> readFile fullPath
        else
          pure Nothing
    }

findTemplate :: String -> [RepoRef] -> IO (Maybe Template)
findTemplate templateName [] = pure Nothing
findTemplate templateName (repo:repos) = do
  foundMay <- findTemplateIn templateName repo
  case foundMay of
    Nothing -> findTemplate templateName repos
    x -> pure x

findTemplateIn :: String -> RepoRef -> IO (Maybe Template)
findTemplateIn templateName (LocalRepo dirname) = do
  let templateDir = dirname </> templateName
      fullPath = templateDir </> "files"
  exists <- doesFileExist fullPath
  if exists then
    pure . Just . fsTemplate $ templateDir
  else
    pure Nothing
