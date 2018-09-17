module Bencode (Value, decode, encode) where

import Data.Map as Map
import Data.ByteString as BS
import Data.ByteString.Lazy as BL
import Data.ByteString.Builder
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator
import Control.Applicative ((<|>))

data Value = BString BS.ByteString | 
             BInt Int | 
             BList [Value] | 
             BDict (Map.Map BS.ByteString Value) deriving (Eq, Show) 

decode :: BS.ByteString -> Maybe Value
decode = maybeResult . parse bvalue

encode :: Value -> BS.ByteString
encode (BString str) = BL.toStrict . toLazyByteString $ intDec (BS.length str) <> char7 ':' <> byteString str
encode (BInt i) = BL.toStrict . toLazyByteString $ char7 'i' <> intDec i <> char7 'e'
encode (BList list) = BL.toStrict . toLazyByteString $ char7 'l' <> byteString (Prelude.foldl BS.append BS.empty $ fmap encode $ list) <> char7 'e'
encode (BDict dict) = BL.toStrict . toLazyByteString $ char7 'd' <> byteString (Prelude.foldl BS.append BS.empty $ fmap encode $ values) <> char7 'e'
    where values = unpair (fmap (\(bs, v) -> (BString bs, v)) . Map.toAscList $ dict) 

unpair :: [(a, a)] -> [a]
unpair [] = []
unpair ((one, two):xs) = one:two:(unpair xs)

bvalue :: Parser Value
bvalue = bint <|> bstring <|> blist <|> bdict

bint :: Parser Value
bint = do
    char 'i'
    value <- signed decimal
    char 'e'
    return . BInt $ value

bstring :: Parser Value
bstring = do
    size <- decimal
    char ':'
    fmap BString $ Data.Attoparsec.ByteString.Char8.take size

blist :: Parser Value
blist = do
    char 'l'
    list <- many1 bvalue
    char 'e'
    return . BList $ list

bdict :: Parser Value
bdict = do
    char 'd'
    pairs <- many1 bpair
    char 'e'
    return . BDict . Map.fromList $ pairs

bpair :: Parser (BS.ByteString, Value)
bpair = do
    (BString key) <- bstring
    value <- bvalue
    return (key, value)