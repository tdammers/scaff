{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE DeriveFunctor #-}
module Scaff.Mapping
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

data MappingOf a =
  Mapping
    { template :: FilePath
    , path :: a
    }
    deriving (Functor)

type Mapping = MappingOf FilePath

type Mappings = [Mapping]

type Context = HashMap Text Value

type RunPure = Run SourcePos (Writer Text) Text

type RunIO = Run SourcePos IO Text

runMapping :: FilePath -> Context -> Mapping -> IO ()
runMapping templateRootDir context mapping = do
  let nullResolver = const . pure $ Nothing
      loadResolver fn =
        (Just <$> readFile fn)
        `catch`
        (\(err :: SomeException) -> hPutStrLn stderr (show err) >> pure Nothing)
      expandFn :: FilePath -> IO FilePath
      expandFn fnRaw = do
        parseGinger nullResolver Nothing fnRaw >>= \case
          Left err ->
            error . show $ err
          Right t ->
            return . Text.unpack $ easyRender (toGVal context :: GVal RunPure) t
  srcFn <- (templateRootDir </>) <$> expandFn (template mapping)
  dstFn <- expandFn (path mapping)
  printf "%s\n" dstFn
  createDirectoryIfMissing True (takeDirectory dstFn)
  parseGingerFile loadResolver srcFn >>= \case
    Left err ->
      hPutStrLn stderr . show $ err
    Right t -> do
      withFile dstFn WriteMode $ \h -> do
        easyRenderM (Text.hPutStr h) (toGVal context :: GVal RunIO) t >>= \case
          Left err ->
            hPutStrLn stderr . show $ err
          Right _ ->
            pure ()
