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
import System.IO
import System.Exit
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
import Text.Read
import Text.Printf

import Scaff.Context
import Scaff.Config
import Scaff.TemplateRepo
import Scaff.Mapping
import Scaff.Ginger

parseArgs :: [String] -> IO (String, String, String)
parseArgs [fn, project, dstDir] = do
  return (project, fn, dstDir)
parseArgs [fn, project] =
  parseArgs [fn, project, "." </> project]
parseArgs _ =
  die "Usage: scaff TEMPLATE PROJECT-NAME [OUTPUT-DIR]"

askQuestions :: [Question] -> IO Context
askQuestions = fmap mconcat . mapM askQuestion

askQuestion :: Question -> IO Context
askQuestion (Question key question def ScalarQuestion) = do
  hPutStr stderr question
  hPutStr stderr "? "
  when (def /= "") $
    hPutStr stderr $ "(" ++ Text.unpack def ++ ") "
  hFlush stderr
  answer' <- getLine
  let answer = if answer' == "" then def else Text.pack answer'
  return $ contextSingleton key answer
askQuestion q@(Question key question def YesNoQuestion) = do
  hPutStr stderr question
  hPutStr stderr " (y/n)? "
  hFlush stderr
  answer' <- getLine
  let defBool = case def of
        "y" -> True
        "1" -> True
        _ -> False
  case answer' of
    "" -> return $ contextSingleton key def
    "y" -> return $ contextSingleton key True
    "Y" -> return $ contextSingleton key True
    "1" -> return $ contextSingleton key True
    "n" -> return $ contextSingleton key False
    "N" -> return $ contextSingleton key False
    "0" -> return $ contextSingleton key False
    _ -> do
      hPutStrLn stderr "Invalid choice"
      askQuestion q
askQuestion q@(Question key question def MultiQuestion) = do
  let go xs = do
        hPutStr stderr question
        hPutStr stderr " (empty line to end)"
        hPutStr stderr "? "
        answer <- getLine
        if answer == "" then
          return xs
        else
          go (answer:xs)
  contextSingleton key . reverse <$> go []
askQuestion q@(Question key question def (MultipleChoiceQuestion options)) = do
  hPutStrLn stderr question
  forM_ (zip [1..] options) $ \(index :: Int, option) -> do
    hPutStrLn stderr $ printf "  %i. %s" index option
  hPutStr stderr "your choice? "
  answer' <- getLine
  if answer' == "" then
    return $ contextSingleton key def
  else
    case readMaybe answer' >>= (options `at1`) of
      Nothing -> do
        hPutStrLn stderr "invalid choice"
        askQuestion q
      Just answer -> do
        return $ contextSingleton key answer
        
-- | Safely get the nth element (1-based) of a list, or Nothing for indexes out
-- of bounds.
at1 :: [a] -> Int -> Maybe a
at1 [] n = Nothing
at1 xs n | n < 1 = Nothing
at1 (x:xs) 1 = Just x
at1 (_:xs) n = at1 xs (pred n)

main :: IO ()
main = do
  args <- getArgs
  (project, templateName, dstDir) <- parseArgs args
  config <- loadConfig
  template <- maybe (die $ "Template not found: " ++ templateName) pure
               =<< findTemplate templateName (templateRepos config)
  questions <- readTemplateQuestions template
  extraVars <- askQuestions questions
  print extraVars
  context <- getContext project extraVars (configVars config)
  mappings <- map (fmap (dstDir </>))
                . catMaybes
                . map parseMapping
                . lines
                . Text.unpack
                <$> ( gingerPure context
                      =<<
                      readTemplateFileOrElse
                        template
                        (die "Could not find template files listing")
                        "files"
                    )
  mapM_ (runMapping template context) mappings
