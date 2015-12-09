#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle --package wreq

{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Foldl as Fold
import Control.Lens ((^.))
import Control.Monad (when)
import Data.ByteString.Lazy (hPut)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time.Format (formatTime, defaultTimeLocale)
import Network.Wreq (get, responseBody)
import Turtle


main = do
  dateStr <- T.pack . formatTime defaultTimeLocale "%Y%m%d" <$> date

  let url = "http://ghcjs.luite.com/master-"<>dateStr<>".tar.gz"
  echo $ format ("Downloading "%s%"...") url
  r <- get (T.unpack url)

  let tempdir = "temp-ghcjs-archive"
  echo $ format ("Using temp directory: "%fp) tempdir
  alreadyThere <- testdir tempdir
  when alreadyThere $ rmtree tempdir
  mkdir tempdir

  let tempfile = tempdir</>"archive.tar.gz"
  echo $ format ("Writing to "%fp%"...") tempfile
  with (writeonly tempfile) $ \temphandle->
    hPut temphandle (r ^. responseBody)

  echo "Unpacking..."
  proc "tar" ["-xzf", format fp tempfile, format ("-C"%fp) tempdir] empty
    .||. die "failed to unpack archive"

  Just ghcjsDir <- fold
    (ls tempdir)
    (Fold.find (T.isPrefixOf "ghcjs-" . format fp . filename))

  let newFolderName = ghcjsDir <.> dateStr
  let ghcjsStackVersion = format (fp%"_ghc-7.10.2") (filename newFolderName)
  let outputArchive = tempdir </> fromText ghcjsStackVersion <.> "tar.gz"

  mv ghcjsDir newFolderName
  echo $ format ("Archiving to "%fp) outputArchive

  targzPack (parent newFolderName) (filename newFolderName) outputArchive

  -- Install archive to the approprate stack folder
  Just outDir <- fold (inproc "stack" ["path", "--ghc-paths"] empty) Fold.head
  echo $ format ("Copying to "%s) outDir
  cp outputArchive (fromText outDir </> filename outputArchive)

  echo $ format ("DONE: Add 'compiler: "%s%"' or 'resolver: "%s%"' to your stack.yaml and run 'stack setup' to boot it.")
                ghcjsStackVersion ghcjsStackVersion

targzPack inDir inFile outFilePath =
    proc "tar" ["-czf", format fp outFilePath, format ("-C"%fp) inDir, format fp inFile] empty
      .||. die (format ("failed to create archive: "%fp) outFilePath)
