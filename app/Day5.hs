module Day5 where

import           Crypto.Hash.MD5       (hash)
import           Data.Bits
import           Data.ByteString       (ByteString, drop, head, isPrefixOf,
                                        pack)
import qualified Data.ByteString.Char8 as Char8
import           Data.Char
import qualified Data.IntMap           as IntMap
import           Data.Monoid
import           Data.Word             (Word8)
import           Prelude               hiding (drop, head, (!!))

indexed :: ByteString -> Int -> ByteString
indexed s n = s <> Char8.pack (show n)

isGood1 :: ByteString -> Bool
isGood1 bs = isPrefixOf (pack [0, 0]) bs && bs !! 2 .&. 0xf0 == 0

isGood2 :: ByteString -> Bool
isGood2 bs = isGood1 bs && ((bs !! 2) .&. 0x0f) <= 7

(!!) :: ByteString -> Int -> Word8
bs !! i = head (drop i bs)

digitFromByte :: Word8 -> Char
digitFromByte digit =
  let c =
        if digit < 0xa
          then digit + 0x30
          else (digit - 10) + 0x61
  in chr (fromIntegral c)

passwordChar1 :: ByteString -> Char
passwordChar1 bs = digitFromByte ((bs !! 2) .&. 0x0f)

passwordChar2 :: ByteString -> Char
passwordChar2 bs = digitFromByte ((bs !! 3) `shift` (-4))

part1 :: [ByteString] -> String
part1 hashes =
  let validHashes = take 8 $ filter isGood1 hashes
  in map passwordChar1 validHashes

part2 :: [ByteString] -> String
part2 hashes =
  let validHashes = filter isGood2 hashes
      go m (h:hs)
        | IntMap.size m == 8 = IntMap.elems m
        | otherwise =
          let pos = fromIntegral $ (h !! 2) .&. 0x0f
          in go (IntMap.insertWith (flip const) pos h m) hs
      go m [] = IntMap.elems m
  in map passwordChar2 (go IntMap.empty validHashes)

main :: IO ()
main = do
  let doorId = Char8.pack "ugkcyxxp"
      candidates = map (indexed doorId) [0 ..]
      hashes = map hash candidates
  putStrLn $ part1 hashes
  putStrLn $ part2 hashes
