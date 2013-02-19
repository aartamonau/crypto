{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Char8 (ByteString, hGet)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Base16 (encode)

import System.IO (Handle, IOMode (ReadMode), SeekMode(AbsoluteSeek),
                  openBinaryFile, hFileSize, hSeek, hIsEOF)
import System.Environment (getArgs)

import Codec.Digest.SHA (hash, Length(SHA256))

type Digest = ByteString

digests :: Handle -> IO [Digest]
digests handle = do
  size <- hFileSize handle
  let pos = ((size - 1) `div` 1024) * 1024
  go pos "" []

  where go pos h r
          | pos < 0 = return r
          | otherwise = do
            hSeek handle AbsoluteSeek pos
            block <- hGet handle 1024
            let block' = BS.concat [block, h]
            let newH = hash SHA256 block'
            go (pos - 1024) newH (newH : r)

verify :: Handle -> [Digest] -> IO Bool
verify _ [] = return False
verify handle (h:hs) = do
  hSeek handle AbsoluteSeek 0
  go h hs

  where go h [] = goBlock h ""
        go prevH (h:hs) = do
          recur <- goBlock prevH h
          if recur
            then go h hs
            else return False

        goBlock h suffix = do
          eof <- hIsEOF handle

          if eof
            then return False
            else do
              block <- hGet handle 1024
              let block' = BS.concat [block, suffix]
              if h == hash SHA256 block'
                then return True
                else return False


main :: IO ()
main = do
  [path] <- getArgs
  handle <- openBinaryFile path ReadMode

  ds <- digests handle
  verified <- verify handle ds

  putStrLn $ "Verification result: " ++ show verified
  putStrLn $ "Initial digest: " ++ BS.unpack (encode $ head ds)
