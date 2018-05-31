{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE DeriveFunctor #-}
module Scaff.Ginger
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Text.Ginger
import Data.Aeson (Value (..))
import System.FilePath
import System.IO
import System.Directory
import Text.Printf
import Control.Monad.Writer
import Control.Exception

import Scaff.Context

type RunPure = Run SourcePos (Writer Text) Text

type RunIO = Run SourcePos IO Text

-- | Expand a Ginger template in a pure fashion, that is, no includes, and no
-- effects. 'IO' is involved only for erroring out.
gingerPure :: Context -> String -> IO Text
gingerPure context src = do
  parseGinger nullResolver Nothing src >>= \case
    Left err ->
      error . show $ err
    Right template ->
      return $ easyRender (toGVal context :: GVal RunPure) template

-- | Expand a Ginger template into a file.
-- Includes are resolved according to the given resolver.
gingerIO :: IncludeResolver IO -> Context -> FilePath -> FilePath -> IO ()
gingerIO resolver context srcFn dstFn = do
  parseGingerFile resolver srcFn >>= \case
    Left err ->
      hPutStrLn stderr . show $ err
    Right t -> do
      withFile dstFn WriteMode $ \h -> do
        easyRenderM (Text.hPutStr h) (toGVal context :: GVal RunIO) t >>= \case
          Left err ->
            hPutStrLn stderr . show $ err
          Right _ ->
            pure ()

-- | Expand a Ginger template from a file, writing output into another file.
-- Includes are resolved relative to the CWD.
gingerFileIO :: Context -> FilePath -> FilePath -> IO ()
gingerFileIO = gingerIO loadResolver

nullResolver :: Monad m => IncludeResolver m
nullResolver = const . pure $ Nothing

loadResolver :: IncludeResolver IO
loadResolver fn =
  (Just <$> readFile fn)
  `catch`
  (\(err :: SomeException) -> hPutStrLn stderr (show err) >> pure Nothing)
