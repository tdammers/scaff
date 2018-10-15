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
import System.Process
import Control.Exception
import Control.Monad
import System.IO.Error
import Data.Char
import Data.List
import Data.List.Split
import System.FilePath.Glob (glob)
import Debug.Trace
import Data.Digest.Pure.SHA (sha1, showDigest)
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import Text.Printf (printf)

import Scaff.HTTP

data RepoRef
  = LocalRepo FilePath
  | HttpRepo String

pprRepo :: RepoRef -> String
pprRepo (HttpRepo url) = url
pprRepo (LocalRepo dir) = dir

listRepos :: IO [(String, RepoRef)]
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

  concat <$> mapM collectRepos repoLists

collectRepos :: FilePath -> IO [(String, RepoRef)]
collectRepos fn = do
  exists <- doesFileExist fn
  if exists then
    fmap catMaybes . mapM parseRepoLine
    =<<
    (lines <$> readFile fn)
  else do
    return []

parseRepoLine :: String -> IO (Maybe (String, RepoRef))
parseRepoLine str =
  case tyName of
    "local" ->
      return $ Just (repoName, LocalRepo arg)
    "http" ->
      return $ Just (repoName, HttpRepo arg)
    "" ->
      return Nothing
    x -> do
      hPutStrLn stderr $ "Warning: unrecognized repository type '" ++ tyName ++ "'"
      return Nothing
  where
    strWithoutComments = takeWhile (/= '#') . dropWhile isSpace $ str
    parts = words strWithoutComments
    (repoName, tyName, arg) = case parts of
      [tyName, arg] -> (tyName ++ "-" ++ hash arg, tyName, arg)
      [repoName, tyName, arg] -> (repoName, tyName, arg)
      _ -> ("", "", "")
    hash = take 6 . showDigest . sha1 . LUTF8.fromString

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

listTemplates :: RepoRef -> IO [FilePath]
listTemplates (HttpRepo rootUrl) = do
  client <- mkHttpClient
  let fullUrl = rootUrl ++ "/templates.list"
  maybe [] lines <$> readFileHttp client fullUrl

listTemplates (LocalRepo templateDir) = do
  map (removePrefix templateDir . takeDirectory) <$> glob (templateDir </> "**" </> "files")

removePrefix :: String -> String -> String
removePrefix pf str
  | pf `isPrefixOf` str = drop (length pf) str
  | otherwise = str

listAllTemplates :: [(String, RepoRef)] -> IO [(String, FilePath)]
listAllTemplates repos =
  concat <$> mapM (\(repoName, repo) -> map (repoName,) . sort <$> listTemplates repo) repos

findTemplate :: String -> [(String, RepoRef)] -> IO (Maybe Template)
findTemplate templateName repos | ':' `elem` templateName = do
  let repoName = takeWhile (/= ':') templateName
      templateName' = (drop 1 . dropWhile (/= ':')) templateName
  printf "%s [%s]\n" templateName' repoName
  case lookup repoName repos of
    Nothing -> return Nothing
    Just repo -> findTemplateIn templateName' repo
findTemplate templateName [] = pure Nothing
findTemplate templateName ((repoName, repo):repos) = do
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

