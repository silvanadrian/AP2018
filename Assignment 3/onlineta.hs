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
import Data.List ( findIndex )
import Control.Monad

formURL :: String
formURL = "https://find.incorrectness.dk/grade/"

showUsage :: IO ()
showUsage =
  putStrLn "Usage: ./onlineta.hs <assignment number> <handin/src directory>"

checkDir :: FilePath -> FilePath -> IO ()
checkDir dir dirExpected = do
  when (takeFileName dir /= dirExpected) $
    fail $ "The hand-in/src directory must be called \"" ++ dirExpected ++ "\""
  dirExists     <- SD.doesDirectoryExist dir
  if dirExists
  then return ()
  else showUsage >> fail ("The directory " ++ dir ++ " does not exist.")

archiveDir :: FilePath -> FilePath -> IO ()
archiveDir dir zipFile = do
      files <- SD.listDirectory dir
      let files' = map ((dir ++ "/") ++ ) $ filter (\f -> head f /= '.') files
      arch <- Zip.addFilesToArchive [Zip.OptRecursive] Zip.emptyArchive files'
      BS.writeFile zipFile (Zip.fromArchive arch)

checkSubmission :: FilePath -> String -> IO ()
checkSubmission zipFile url = do
  manager <- newManager tlsManagerSettings
  request <- parseUrlThrow url
  let files = [partFileSource (fromString "src") zipFile]
  body <- formDataBody files request
  response <- responseBody <$> httpLbs body manager
  BS.putStr response
  return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [assignment, dir] ->
      case findIndex ((read assignment ==) . assNum) assignments of
        Just i  -> do
          let ass = assignments!!i
          checkDir dir $ handinDir ass
          SD.setCurrentDirectory $ zipParent ass dir
          archiveDir (zipDir ass) (zipName ass)
          checkSubmission (zipName ass) (formURL ++ show (assNum ass))
        Nothing -> fail "Sorry, that assignment isn't supported yet."

    _ -> showUsage

data Ass = Ass { assNum    :: Int
               , handinDir :: FilePath
               , zipParent :: FilePath -> FilePath -- parent relative to the handinDir
               , zipDir    :: FilePath             -- relative to zipParent
               , zipName   :: FilePath
               }

assignments :: [Ass]
assignments = [ Ass 0 "src"    dropFileName "src"    "src.zip"
              , Ass 1 "handin" id           "src"    "src.zip"
              , Ass 2 "handin" dropFileName "handin" "handin.zip"
              , Ass 3 "handin" dropFileName "handin" "handin.zip"
              ]
