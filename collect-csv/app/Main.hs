{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Applicative
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Csv as Csv
import qualified Data.Map as Map
import Data.Text (Text, pack, strip, unpack)
import Data.Yaml
import GHC.Generics
import Lowarn.Cli.Config
import Lowarn.Cli.Env
import Lowarn.Cli.Retrofit.Directory
import Lowarn.Cli.VersionGraph
import Lowarn.Cli.VersionPath
import Lowarn.VersionNumber
import Path
import Path.IO
import System.Process
import Text.Printf

data VersionInfo = VersionInfo
  { versionInfoCommit :: !Text,
    versionInfoType :: !Text,
    versionInfoReleaseVersion :: !Text
  }
  deriving (Show)

instance FromJSON VersionInfo where
  parseJSON :: Value -> Parser VersionInfo
  parseJSON = withObject "VersionInfo" $ \v ->
    liftA3
      VersionInfo
      (v .: "commit")
      (v .: "type")
      (v .: "version")

newtype ClocCount = ClocCount {unClocCount :: Int}

instance FromJSON ClocCount where
  parseJSON :: Value -> Parser ClocCount
  parseJSON = withObject "ClocCount" $ \v -> ClocCount <$> (v .: "code")

newtype ClocFile = ClocFile {unClocFile :: ClocCount}

instance FromJSON ClocFile where
  parseJSON :: Value -> Parser ClocFile
  parseJSON = withObject "ClocFile" $ \v -> ClocFile <$> (v .: "SUM")

data ClocDiff = ClocDiff
  { clocDiffModified :: !ClocCount,
    clocDiffAdded :: !ClocCount,
    clocDiffRemoved :: !ClocCount
  }

instance FromJSON ClocDiff where
  parseJSON :: Value -> Parser ClocDiff
  parseJSON = withObject "ClocDiff" $ \v ->
    liftA3 ClocDiff (v .: "modified") (v .: "added") (v .: "removed")

newtype ClocDiffFile = ClocDiffFile {unClocDiffFile :: ClocDiff}

instance FromJSON ClocDiffFile where
  parseJSON :: Value -> Parser ClocDiffFile
  parseJSON = withObject "ClocFile" $ \v -> ClocDiffFile <$> (v .: "SUM")

data VersionRecord = VersionRecord
  { versionRecordVersionNumber :: !Text,
    versionRecordCommit :: !Text,
    versionRecordDate :: !Text,
    versionRecordType :: !Text,
    versionRecordReleaseVersion :: !Text,
    versionRecordSimplifiedLines :: !Int,
    versionRecordRetrofittedLines :: !Int,
    versionRecordModifiedLines :: !Int,
    versionRecordAddedLines :: !Int,
    versionRecordRemovedLines :: !Int
  }
  deriving (Generic, Show)

instance Csv.FromRecord VersionRecord

instance Csv.ToRecord VersionRecord

rootDirectory :: Path Abs Dir
rootDirectory = [absdir|/|]

main :: IO ()
main = do
  workingDirectory <- getCurrentDir
  searchDirectory <- stripProperPrefix rootDirectory workingDirectory
  Just configPath <- findConfigPath rootDirectory searchDirectory
  Right env <- getLowarnEnv configPath
  let projectDirectory = parent configPath
      programName = lowarnConfigProgramName $ lowarnEnvConfig env
  versionGraph <-
    getVersionGraph projectDirectory programName [reldir|retrofitted|]
  let versionsAndVersionPaths =
        map
          (\v -> (v, versionNumberToPath projectDirectory v))
          (Map.keys $ unVersionGraph versionGraph)
  versionRecords <-
    mapM (uncurry $ getVersionRecord env) versionsAndVersionPaths
  ByteString.writeFile
    (toFilePath $ projectDirectory </> [relfile|data.csv|])
    (Csv.encode versionRecords)
  return ()

getVersionRecord ::
  LowarnEnv -> VersionNumber -> Path Abs Dir -> IO VersionRecord
getVersionRecord env versionNumber versionDirectory = do
  let versionRecordVersionNumber = pack $ showWithDots versionNumber
  VersionInfo {..} <-
    decodeFileEither
      ( toFilePath $
          versionDirectory </> [relfile|version-info.yaml|]
      )
      >>= \case
        Left e ->
          fail $
            printf
              "Could not parse version info file at %s: %s"
              (toFilePath versionDirectory)
              (show e)
        Right versionInfo -> return versionInfo
  let versionRecordCommit = versionInfoCommit
      versionRecordType = versionInfoType
      versionRecordReleaseVersion = versionInfoReleaseVersion
  versionRecordDate <-
    strip . pack
      <$> withRetrofitDirectory
        env
        ( \retrofitDirectory ->
            readCreateProcess
              ( ( proc
                    "git"
                    ["show", unpack versionInfoCommit, "--format=%aI", "-s"]
                )
                  { cwd =
                      Just $ toFilePath $ retrofitDirectory </> [reldir|repo|]
                  }
              )
              ""
        )
  _ <-
    readCreateProcess
      ( ( proc
            "cloc"
            [ "--include-lang",
              "haskell",
              "--count-and-diff",
              "simplified",
              "retrofitted",
              "--yaml",
              "--sum-one",
              "--quiet",
              "--report-file",
              "cloc.yaml"
            ]
        )
          { cwd = Just $ toFilePath versionDirectory
          }
      )
      ""
  versionRecordSimplifiedLines <-
    unClocCount . unClocFile
      <$> decodeFileThrow
        (toFilePath $ versionDirectory </> [relfile|cloc.yaml.simplified|])
  versionRecordRetrofittedLines <-
    unClocCount . unClocFile
      <$> decodeFileThrow
        (toFilePath $ versionDirectory </> [relfile|cloc.yaml.retrofitted|])
  ClocDiff {..} <-
    unClocDiffFile
      <$> decodeFileThrow
        ( toFilePath $
            versionDirectory
              </> [relfile|cloc.yaml.diff.simplified.retrofitted|]
        )
  let versionRecordModifiedLines = unClocCount clocDiffModified
      versionRecordAddedLines = unClocCount clocDiffAdded
      versionRecordRemovedLines = unClocCount clocDiffRemoved
  return VersionRecord {..}
