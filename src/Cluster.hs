{-# LANGUAGE OverloadedStrings #-}

module Cluster (app) where

import Control.Monad ((<=<))
import qualified Data.ByteString as BS
import Data.Foldable (for_)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Traversable (for)
import qualified Dhall
import qualified Dhall.JSON.Yaml as DhallYaml
import qualified Dhall.Yaml as Dhall
import Lens.Family ((&), (.~))
import qualified Prettyprinter as Pretty
import qualified Prettyprinter.Render.Text as Pretty
import qualified System.Directory as Directory
import System.IO (hClose)
import qualified System.IO.Temp as Temp

newtype Folder = Folder {unFolder :: FilePath}
  deriving (Show)

newtype File a = File {unFile :: FilePath}
  deriving (Show)

data Dhall

data Yaml

dirContents :: Folder -> IO [Either (File Text) Folder]
dirContents (Folder dir) = do
  stuff <- Directory.listDirectory dir
  for stuff $ \path -> do
    let inDir = dir <> "/" <> path
    isFile <- Directory.doesFileExist inDir
    pure $
      if isFile
        then Left (File inDir)
        else Right (Folder inDir)

dirFolders :: Folder -> IO [Folder]
dirFolders = fmap (concat . map (either (const []) (: []))) . dirContents

dirFiles :: Folder -> IO [File Text]
dirFiles = fmap (concat . map (either (: []) (const []))) . dirContents

withExtension :: Text -> File Text -> Bool
withExtension ext (File f) = ext `Text.isSuffixOf` (Text.pack f)

dhallFile :: File Text -> Maybe (File Dhall)
dhallFile file
  | withExtension ".dhall" file = Just $ File (unFile file)
  | otherwise = Nothing

kubectlDelete :: File Yaml -> IO ()
kubectlDelete (File config) =
  putStrLn $ "deleting kube object from " <> config

kubectlApply :: File Yaml -> IO ()
kubectlApply (File config) =
  putStrLn $ "applying configuration from " <> config

updateKubeObject :: File Yaml -> IO ()
updateKubeObject config = do
  kubectlDelete config
  kubectlApply config

-- | Takes a dhall file and resolves imports, normalizes,
-- typechecks, and converts yaml, and writes to a temp file
-- which is deleted after use.
withCompiledDhallConfig :: (File Yaml -> IO a) -> Folder -> File Dhall -> IO a
withCompiledDhallConfig go service (File file) = do
  let settings =
        Dhall.defaultInputSettings
          & Dhall.rootDirectory .~ (unFolder service)
  let options = DhallYaml.defaultOptions
  putStrLn . ("compiling " <>) =<< Directory.makeRelativeToCurrentDirectory file
  tmpDir <- Directory.getTemporaryDirectory
  yaml <-
    Dhall.dhallToYaml options Nothing
      . Pretty.renderStrict
      . Pretty.layoutPretty Pretty.defaultLayoutOptions
      . Pretty.pretty
      =<< Dhall.inputExprWithSettings settings
      =<< Text.readFile file
  Temp.withTempFile tmpDir "dhall-config.yaml" $ \path handle -> do
    BS.hPut handle yaml
    hClose handle
    go $ File path

app :: IO ()
app = do
  thisDir <- Folder <$> Directory.getCurrentDirectory
  services <- dirFolders thisDir
  putStrLn "Detected Services:"
  for_ services $
    putStrLn . ("- " <>)
      <=< Directory.makeRelativeToCurrentDirectory
        . unFolder
  for_ services $ \service -> do
    files <- dirFiles service
    traverse (withCompiledDhallConfig updateKubeObject service)
      . catMaybes
      $ map dhallFile files
