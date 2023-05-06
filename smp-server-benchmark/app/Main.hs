{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception hiding (handle)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Foldable
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Time.Clock.POSIX
import Simplex.Messaging.Agent
import Simplex.Messaging.Agent.Client (closeAgentClient)
import Simplex.Messaging.Agent.Env.SQLite
  ( InitialAgentServers (InitialAgentServers),
    createAgentStore,
    defaultAgentConfig,
  )
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Agent.Store.SQLite
import Simplex.Messaging.Client
import Simplex.Messaging.Protocol
import System.Environment
import System.IO.Temp
import System.Process
import Text.Printf

dataPath, projectPath :: FilePath
dataPath = "[Directory name here]"
projectPath = "[Absolute path to project here]"

versions :: [Int]
versions = [268, 289, 300, 308, 317, 327, 338, 356]

smpServerWithAuth :: SMPServerWithAuth
smpServerWithAuth =
  "smp://01w5YdCsOIu_CJZnOcCbcGMMHY2Wo6CCrFs2xtrhqqI@localhost:5223"

servers :: InitialAgentServers
servers =
  InitialAgentServers
    (Map.fromList [(1, smpServerWithAuth :| [])])
    []
    Map.empty
    defaultNetworkConfig

createAgent :: FilePath -> IO AgentClient
createAgent storePath = do
  Right store <- createAgentStore storePath "" MCError
  threadDelay 1000000
  client <- getSMPAgentClient defaultAgentConfig servers store
  threadDelay 1000000
  return client

get :: AgentClient -> IO (ACommand 'Agent 'AEConn)
get client = do
  (_, _, APC entity cmd) <- atomically $ readTBQueue $ subQ client
  case cmd of
    CONNECT {} -> get client
    DISCONNECT {} -> get client
    _ -> case entity of
      SAEConn -> return cmd
      _ -> error $ "unexpected command " <> show cmd

benchmarkAgents :: Bool -> [(AgentClient, AgentClient)] -> IO (Double, [String])
benchmarkAgents isCli clientPairs = do
  memoryQueueVar <- newMVar Seq.empty
  let writeToMemoryChannel = forever $ do
        usage <-
          readCreateProcess
            ( shell $
                printf
                  "smem -c \"uss pss rss\" -P '%s' | tail -n 1 | sed -E 's/^\\s+//;s/\\s+$//;s/\\s+/,/g'"
                  nameFilter
            )
            ""
        modifyMVar_ memoryQueueVar (return . (Seq.:|> usage))
        threadDelay 500000
  startTimestamp <- getPOSIXTime

  void $
    race
      ( forConcurrently_ clientPairs $
          \(alice, bob) -> do
            Right () <-
              runExceptT $ do
                (bobId, qInfo) <-
                  createConnection alice 1 True SCMInvitation Nothing
                aliceId <- joinConnection bob 1 True qInfo "bob's connInfo"
                CONF confId _ "bob's connInfo" <- liftIO $ get alice
                allowConnection alice bobId confId "alice's connInfo"
                void $ liftIO $ get alice >> get bob >> get bob
                idsToAck <- replicateM 100 $ do
                  _ <- sendMessage alice bobId noMsgFlags "hello"
                  SENT idToAck <- liftIO $ get alice
                  return idToAck
                forM_ idsToAck $ \idToAck -> do
                  _ <- liftIO $ get bob
                  ackMessage bob aliceId idToAck
                suspendConnection alice bobId
                deleteConnection alice bobId
                suspendConnection bob aliceId
                deleteConnection bob aliceId
            return ()
      )
      writeToMemoryChannel

  endTimestamp <- getPOSIXTime
  memoryLog <- toList <$> readMVar memoryQueueVar

  return (realToFrac $ endTimestamp - startTimestamp, memoryLog)
  where
    nameFilter :: String
    nameFilter = if isCli then "lowarn-cli" else "smp-server(?!-b)"

benchmark :: Bool -> IO (Double, [String])
benchmark isCli = do
  withSystemTempDirectory "stores" $ \storesPath ->
    bracket
      ( mapConcurrently
          ( \(i :: Int) -> do
              alice <- createAgent $ storesPath <> show (i * 2)
              bob <- createAgent $ storesPath <> show (i * 2 + 1)
              return (alice, bob)
          )
          [1 .. 50]
      )
      ( mapM $ \(alice, bob) -> do
          closeAgentClient alice
          closeAgentClient bob
      )
      (benchmarkAgents isCli)

withNormalProcess :: Int -> IO a -> IO a
withNormalProcess !versionNumber !f =
  withCreateProcess (proc ("smp-server-" <> show versionNumber) ["start"]) $
    \_ _ _ _ -> f

withLowarnCliProcess :: Int -> IO a -> IO a
withLowarnCliProcess !versionNumber !f =
  withCreateProcess
    ( ( proc
          "lowarn-cli"
          ["run", "--version", show versionNumber]
      )
        { env =
            Just
              [ ( "LOWARN_PACKAGE_ENV",
                  projectPath <> "/.ghc.environment.x86_64-linux-9.2.7"
                )
              ],
          cwd = Just projectPath
        }
    )
    $ \_ _ _ _ -> f

writeBenchmarkFiles :: Bool -> Int -> Int -> IO ()
writeBenchmarkFiles !isCli !versionNumber !number =
  withProcess $ do
    threadDelay 1000000
    (!time, !memoryLog) <- benchmark isCli
    writeFile (getOutputPath "time") (show time <> "\n")
    writeFile (getOutputPath "memory") (concat memoryLog)
  where
    getOutputPath :: String -> FilePath
    getOutputPath name =
      dataPath
        <> "results/"
        <> show versionNumber
        <> "-"
        <> lowarnOrNormal
        <> "-"
        <> show number
        <> "-"
        <> name
        <> ".txt"

    lowarnOrNormal = if isCli then "lowarn" else "normal"

    withProcess :: IO a -> IO a
    withProcess =
      (if isCli then withLowarnCliProcess else withNormalProcess) versionNumber

setups :: [(Bool, Int, Int)]
setups =
  [ (isCli, versionNumber, number)
    | number <- [0 .. 9],
      versionNumber <- versions,
      isCli <- [True, False]
  ]

main :: IO ()
main =
  getArgs >>= \case
    [isCli, versionNumber, number] ->
      writeBenchmarkFiles (read isCli) (read versionNumber) (read number)
    _ ->
      forM_ setups $ \(!isCli, !versionNumber, !number) -> do
        putStrLn $ "Running " <> show (isCli, versionNumber, number)
        (Nothing, Nothing, Nothing, processHandle) <- createProcess
          ( shell $
              printf
                "smp-server-benchmark-exe %s %d %d"
                (show isCli)
                versionNumber
                number
          )
        exitCode <- waitForProcess processHandle
        print exitCode
        putStrLn ""
        threadDelay $ 30 * 1000000
