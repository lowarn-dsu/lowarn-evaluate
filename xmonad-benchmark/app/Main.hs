{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.Bits
import Data.Foldable
import qualified Data.Sequence as Seq
import Data.Time.Clock.POSIX
import Graphics.X11.XTest
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import System.IO
import System.Process
import System.Timeout
import Text.Printf

dataPath, instructionsPath, xSessionScriptPath, projectPath, binPath :: FilePath
dataPath = "[Directory name here]"
instructionsPath = dataPath <> "instructions.txt"
xSessionScriptPath = "[Script path here]"
projectPath = "[Absolute path to project here]"
binPath = "[Absolute path to directory containing xmonad binaries here]"

getLowarnCliScript :: String -> String
getLowarnCliScript =
  printf
    "#!/bin/sh\n\nexec env LOWARN_PACKAGE_ENV=%s/.ghc.environment.x86_64-linux-9.2.7 lowarn-cli --lowarn-yaml %s/lowarn.yaml run --version %s\n"
    projectPath
    projectPath

getXmonadScript :: String -> String
getXmonadScript =
  printf "#!/bin/sh\n\nexec env %s/xmonad-%s\n" binPath

benchmark :: Bool -> IO (Double, [String])
benchmark isCli = do
  hSetBuffering stdout NoBuffering

  void $
    readCreateProcess
      (shell $ printf "pgrep -o %s | xargs kill -s SIGSTOP" executableName)
      ""
  threadDelay 1000000

  display <- openDisplay ""
  let sendAltKey modifiers = sendKey display $ xK_Alt_L : modifiers
  replicateM_ 400 $ do
    sendAltKey [] xK_space
    replicateM_ 3 $ sendAltKey [] xK_h
    replicateM_ 3 $ sendAltKey [] xK_l
    sendAltKey [] xK_k
    sendAltKey [xK_Shift_L] xK_2
    sendAltKey [] xK_2
    sendAltKey [xK_Shift_L] xK_1
    sendAltKey [] xK_1
    sendAltKey [] xK_j
    replicateM_ 2 $ sendAltKey [] xK_space

  startTimestamp <- getPOSIXTime
  void $
    readCreateProcess
      (shell $ printf "pgrep -o %s | xargs kill -s SIGCONT" executableName)
      ""

  lastTimestampVar <- newEmptyMVar
  let waitForTimestampToBeEmpty =
        takeMVar lastTimestampVar >>= waitForTimestampToBeEmptyTimeout
      waitForTimestampToBeEmptyTimeout lastTimestamp =
        timeout 1000000 (takeMVar lastTimestampVar)
          >>= \case
            Just t -> waitForTimestampToBeEmptyTimeout t
            Nothing -> return lastTimestamp

  memoryQueueVar <- newMVar Seq.empty
  let writeToMemoryChannel = forever $ do
        usage <-
          readCreateProcess
            ( shell $
                printf
                  "smem -c \"uss pss rss\" -P ^%s | tail -n 1 | sed -E 's/^\\s+//;s/\\s+$//;s/\\s+/,/g'"
                  executableName
            )
            ""
        modifyMVar_ memoryQueueVar (return . (Seq.:|> usage))
        threadDelay 500000

  endTimestamp <-
    race
      ( allocaXEvent $ \xEventPointer -> do
          let window = defaultRootWindow display
          (_, _, childWindows) <- queryTree display window

          selectInput display window $ exposureMask .|. substructureNotifyMask
          mapM_
            (\w -> selectInput display w $ exposureMask .|. structureNotifyMask)
            childWindows

          forever $
            checkMaskEvent
              display
              (exposureMask .|. substructureNotifyMask .|. structureNotifyMask)
              xEventPointer
              >>= \case
                True -> getPOSIXTime >>= putMVar lastTimestampVar
                False -> do
                  flush display
                  threadDelay 100000
      )
      (race writeToMemoryChannel waitForTimestampToBeEmpty)
      >>= \case
        Right (Right t) -> return t
        _ -> fail "This loop should never end."

  memoryLog <- toList <$> readMVar memoryQueueVar

  return (realToFrac $ endTimestamp - startTimestamp, memoryLog)
  where
    executableName = if isCli then "lowarn-cli" else "xmonad"

openTwoTerminals :: IO ()
openTwoTerminals = do
  threadDelay $ 15 * 1000000
  display <- openDisplay ""
  replicateM_ 2 $ sendKey display [xK_Alt_L, xK_Shift_L] xK_Return
  threadDelay $ 15 * 1000000

doBenchmarkInstruction :: Bool -> String -> Integer -> IO ()
doBenchmarkInstruction isCli versionNumber number = do
  openTwoTerminals
  (time, memoryLog) <- benchmark isCli
  let getOutputPath name =
        dataPath
          <> "results/"
          <> versionNumber
          <> "-"
          <> lowarnOrNormal
          <> "-"
          <> show number
          <> "-"
          <> name
          <> ".txt"
  writeFile (getOutputPath "time") (show time <> "\n")
  writeFile (getOutputPath "memory") (concat memoryLog)
  doInstruction
  where
    lowarnOrNormal = if isCli then "lowarn" else "normal"

doInstruction :: IO ()
doInstruction = do
  instructionsFile <- readFile' instructionsPath
  case lines instructionsFile of
    [] -> return ()
    [""] -> return ()
    instruction : instructions -> do
      writeFile instructionsPath $ unlines instructions
      case words instruction of
        ["benchmark-lowarn", versionNumber, number] ->
          doBenchmarkInstruction True versionNumber (read number)
        ["benchmark-xmonad", versionNumber, number] ->
          doBenchmarkInstruction False versionNumber (read number)
        ["set-lowarn", versionNumber] ->
          setScriptAndRestart $ getLowarnCliScript versionNumber
        ["set-xmonad", versionNumber] ->
          setScriptAndRestart $ getXmonadScript versionNumber
        i -> fail $ printf "Did not recognise instruction %s." $ show i
  where
    setScriptAndRestart :: String -> IO ()
    setScriptAndRestart contents = do
      writeFile xSessionScriptPath contents
      void $ readProcess "sudo" ["systemctl", "restart", "gdm.service"] ""

main :: IO ()
main = doInstruction
