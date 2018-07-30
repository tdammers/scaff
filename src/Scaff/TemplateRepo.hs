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
import Control.Monad
import System.IO.Error
import Data.Char
import Data.List
import Data.List.Split

import Scaff.HTTP

data RepoRef
  = LocalRepo FilePath
  | HttpRepo String

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
    "http" ->
      return $ Just (HttpRepo arg)
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

data Question
  = Question
      { questionVar :: Text
      , questionPrompt :: String
      , questionDefault :: Text
      , questionMode :: QuestionMode
      }
      deriving (Show)

data QuestionMode
  = YesNoQuestion
  | ScalarQuestion
  | MultiQuestion
  | MultipleChoiceQuestion [String]
  deriving (Show)

readTemplateFileOrElse :: Template -> IO () -> FilePath -> IO String
readTemplateFileOrElse tpl barf fn =
  readTemplateFile tpl fn >>= maybe (barf >> return "") return

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

readTemplateQuestions :: Template -> IO [Question]
readTemplateQuestions tpl =
  readTemplateFile tpl "questions" >>= maybe (return []) go
  where
    go str =
      catMaybes <$> forM (lines str) (return . parseQuestion)
      where
        parseQuestion :: String -> Maybe Question
        parseQuestion str = case map trim . splitOn ":" $ str of
          [key, question] ->
            go key question "" ""
          [key, question, def, modeStr] ->
            go key question def modeStr
          (key : question : def : modeStr : _) -> do
            go key question def modeStr
          _ ->
            Nothing

        go key question def modeStr =
          Just (Question (Text.pack key) question (Text.pack def) mode)
          where
            mode = case modeStr of
              "" -> ScalarQuestion
              "." -> ScalarQuestion
              "*" -> MultiQuestion
              "?" -> YesNoQuestion
              str ->
                let options = map trim . splitOn "," $ str
                in MultipleChoiceQuestion options

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

httpTemplate :: String -> IO Template
httpTemplate rootUrl = do
  client <- mkHttpClient
  return Template
    { readTemplateFile = \fn -> do
        let fullUrl = rootUrl ++ "/" ++ fn
        readFileHttp client fullUrl
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
findTemplateIn templateName (HttpRepo rootUrl) = do
  let templateRootUrl = rootUrl ++ "/" ++ templateName
  template <- httpTemplate templateRootUrl
  filesBodyMay <- readTemplateFile template "files"
  if isJust filesBodyMay then
    pure . Just $ template
  else
    pure Nothing
