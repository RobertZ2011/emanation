module Main where

import Bencode
import qualified Data.ByteString as BS

main :: IO ()
main = do
    torrent <- BS.readFile "leaves.torrent"
    print . decode $ torrent
