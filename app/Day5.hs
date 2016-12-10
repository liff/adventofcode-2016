module Day5 where

import Crypto.Hash.MD5 (hash)
import Data.Bits
import Data.ByteString (ByteString, drop, head, isPrefixOf, pack)
import qualified Data.ByteString.Char8 as Char8
import Data.Char
import Data.Monoid
import Prelude hiding (drop, head)

indexed :: ByteString -> Integer -> ByteString
indexed s n = s <> (Char8.pack $ show n)

isGood :: ByteString -> Bool
isGood bs = isPrefixOf (pack [0, 0]) bs && ((head $ drop 2 bs) .&. 0xf0) == 0

passwordChar :: ByteString -> Char
passwordChar bs =
    let digit = (head $ drop 2 bs) .&. 0x0f
        c =
            if digit < 0xa
                then digit + 0x30        -- '0'
                else (digit - 10) + 0x61 -- 'a'
    in chr $ fromIntegral c

main :: IO ()
main = do
    let doorId = Char8.pack "ugkcyxxp"
        candidates = zipWith indexed (repeat doorId) [0 ..]
        hashes = map hash candidates
        validHashes = take 8 $ filter isGood hashes
    putStrLn $ map passwordChar validHashes
