#!/usr/bin/env stack
{- stack
   --resolver lts-12.6
   --install-ghc
   runghc
   --package http-client-tls
   --package zip-archive
   --package filepath
-}

-- Check your submission with OnlineTA <https://find.incorrectness.dk/grade/>.
--
-- This script will zip your src directory (must be present!) and try to submit
-- the generated src.zip file to OnlineTA.  Warning: If you already have a
-- src.zip file, this can overwrite files in it.
--
-- Usage: ./onlineta.hs <assignment number> <work directory>
--
-- Example use:
--
-- $ ./onlineta.hs 0 ~/ap/assignments/0
--
-- This will test the files ~/ap/assignments/0 for assignment 0.
--
-- This works thanks to the "shebang" at the top of this file.
--
-- Otherwise, you will need to compile this Haskell file to an executable first:
--
-- $ stack ghc -- -Wall onlineta.hs
-- $ ./onlineta 0 ~/ap/assignments/0

{-# LANGUAGE OverloadedStrings #-}

import qualified Codec.Archive.Zip as Zip
import qualified Data.ByteString.Lazy as BS
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Client.MultipartFormData
import qualified System.Directory as SD
import System.Environment ( getArgs )
import System.FilePath
import Data.String ( fromString )
import Control.Monad

formURL :: String
formURL = "https://find.incorrectness.dk/grade/"

showUsage :: IO ()
showUsage =
  putStrLn "Usage: ./onlineta.hs <assignment number> <handin directory>"

checkDir :: FilePath -> IO ()
checkDir dir = do
  when (takeFileName dir /= "handin") $
    fail "The hand-in directory must be called \"handin\""
  dirExists     <- SD.doesDirectoryExist dir
  srcExists     <- SD.doesDirectoryExist $ dir ++ "/src"
  if dirExists && srcExists
  then return ()
  else do
    showUsage
    unless dirExists $
      fail $ "The directory " ++ dir ++ " does not exist."
    unless srcExists $
      fail $ "The directory " ++ dir ++ " does not contain a src subdirectory."

archiveDir :: String -> IO ()
archiveDir assgn
  | assgn == "1"  = do
      arch <- Zip.addFilesToArchive [Zip.OptRecursive] Zip.emptyArchive ["src"]
      BS.writeFile "src.zip" (Zip.fromArchive arch)
  | otherwise = do
      files <- SD.listDirectory "handin"
      let files' = map ("handin/" ++ ) $ filter (\f -> head f /= '.') files
      arch <- Zip.addFilesToArchive [Zip.OptRecursive] Zip.emptyArchive files'
      BS.writeFile "handin.zip" (Zip.fromArchive arch)

checkSubmission :: String -> String -> IO ()
checkSubmission assign url = do
  manager <- newManager tlsManagerSettings
  request <- parseUrlThrow url
  let files = if assign == "1"
              then [partFileSource (fromString "src") "src.zip"]
              else [partFileSource (fromString "src") "handin.zip"]
  body <- formDataBody files request
  response <- responseBody <$> httpLbs body manager
  BS.putStr response
  return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [assignment, dir] -> do
      checkDir dir
      if assignment == "1"
      then SD.setCurrentDirectory dir
      else SD.setCurrentDirectory $ dropFileName dir
      archiveDir assignment
      checkSubmission assignment (formURL ++ assignment)
    _ -> showUsage
