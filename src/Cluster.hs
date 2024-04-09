{-# LANGUAGE OverloadedStrings #-}

module Cluster (app) where

import Control.Monad ((<=<))
import Data.Foldable (for_, traverse_)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Traversable (for)
import qualified System.Directory as Directory
import qualified System.Process as Process

newtype Folder = Folder {unFolder :: FilePath}

newtype File = File {unFile :: FilePath}

newtype DhallCode = DhallCode {unDhallCode :: Text}

newtype YamlCode = YamlCode {unYamlCode :: Text}

dirContents :: Folder -> IO [Either File Folder]
dirContents (Folder dir) = do
  stuff <- Directory.listDirectory dir
  for stuff $ \path -> do
    isFile <- Directory.doesFileExist path
    pure $
      if isFile
        then Left (File path)
        else Right (Folder path)

dirFolders :: Folder -> IO [Folder]
dirFolders = fmap (concat . map (either (const []) (: []))) . dirContents

dirFiles :: Folder -> IO [File]
dirFiles = fmap (concat . map (either (: []) (const []))) . dirContents

withExtension :: Text -> File -> Bool
withExtension ext (File f) = ext `Text.isSuffixOf` (Text.pack f)

dhallResolve :: DhallCode -> IO DhallCode
dhallResolve = undefined

dhallNormalize :: DhallCode -> IO DhallCode
dhallNormalize = undefined

dhallToYaml :: DhallCode -> IO YamlCode
dhallToYaml = undefined

dhallEval :: DhallCode -> IO DhallCode
dhallEval = undefined

data KubeObject
  = Service
  | Deployment

newtype KubeName = KubeName Text

kubectlDelete :: KubeObject -> KubeName -> IO ()
kubectlDelete = undefined

kubectlApply :: File -> IO ()
kubectlApply = undefined

compileDhallConfig :: File -> IO YamlCode
compileDhallConfig (File file) = do
  config <- DhallCode <$> Text.readFile file
  putStrLn "compiling _"
  putStrLn "> resolving..."
  resolved <- dhallResolve config
  putStrLn "> normalizing..."
  normal <- dhallNormalize resolved
  putStrLn "> converting to yaml..."
  yaml <- dhallToYaml normal
  putStrLn "> done..."
  pure yaml

updateKubeObject :: YamlCode -> IO ()
updateKubeObject = undefined

app :: IO ()
app = do
  thisDir <- Directory.getCurrentDirectory
  services <- dirFolders (Folder thisDir)
  for_ services $
    traverse_ updateKubeObject
      <=< traverse compileDhallConfig
        . filter (withExtension ".dhall")
      <=< dirFiles
