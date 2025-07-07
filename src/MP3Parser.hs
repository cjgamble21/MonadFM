module MP3Parser where

import qualified Data.ByteString.Lazy as B
import Data.Binary.Get
import Data.Bits
import Data.Word
import Data.Maybe

data MP3FrameHeader = MP3FrameHeader {
    bitRate :: Int,
    sampleRate :: Int
} deriving (Show)

extractBits :: (Bits a, Num a) => a -> Int -> Int -> a
extractBits value startIndex size = (value `shiftR` startIndex) .&. ((1 `shiftL` size) - 1)

getMP3FrameHeader :: Word32 -> Maybe MP3FrameHeader
getMP3FrameHeader bs
    | syncWord /= 0x7FF = Nothing
    | mpegVersion /= 3 = Nothing
    | layerVersion /= 1 = Nothing
    | bitRate == 0 || bitRate == 15 = Nothing
    | otherwise = Just $ MP3FrameHeader bitRate sampleRate
    where
        syncWord = extractBits bs 21 11
        mpegVersion = extractBits bs 19 2
        layerVersion = extractBits bs 17 2
        bitRate = fromIntegral $ extractBits bs 12 3
        sampleRate = fromIntegral $ extractBits bs 10 2

-- Try to read a 4-byte Word32 from a given ByteString slice
tryReadWord32 :: B.ByteString -> Word32
tryReadWord32 = runGet getWord32be
-- tryReadWord32 bs
--     | B.length bs < 4 = Nothing
--     | otherwise       = Just $ runGet getWord32be (B.take 4 bs)

windows :: B.ByteString -> [B.ByteString]
windows bs
  | B.length bs < 4 = []
  | otherwise       = bs : windows (B.drop 1 bs)

mp3Seek :: B.ByteString -> Maybe MP3FrameHeader
mp3Seek bs = listToMaybe
  [ hdr
  | chunk <- windows bs
--   , Just word <- [tryReadWord32 chunk]
  , Just hdr  <- [getMP3FrameHeader $ tryReadWord32 bs]
  ]

main :: IO ()
main = do
    input <- B.readFile "./TastyWaves.mp3"
    let frame = mp3Seek input
    print frame