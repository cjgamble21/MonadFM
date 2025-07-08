module MP3Parser where

import qualified Data.ByteString.Lazy as B
import Data.Binary.Get
import Data.Bits
import Data.Word
import Data.Maybe

type MP3DataStream = B.ByteString

data MP3FrameHeader = MP3FrameHeader {
    bitRate :: Int,
    sampleRate :: Int
} deriving (Show)

extractBits :: (Bits a, Num a) => a -> Int -> Int -> a
extractBits value startIndex size = (value `shiftR` startIndex) .&. ((1 `shiftL` size) - 1)

bitRates :: [Int]
bitRates = [0, 32, 48, 56, 64, 80, 96, 112, 128, 144, 160, 176, 192, 224, 256]

sampleRates :: [Int]
sampleRates = [44100, 48000, 32000]

getMP3FrameHeader :: Word32 -> Maybe MP3FrameHeader
getMP3FrameHeader bs
    | syncWord /= 0x7FF = Nothing
    | mpegVersion /= 3 = Nothing -- Only supporting MPEG-1
    | layerVersion /= 1 = Nothing -- Only supporting Layer 3
    | bitRate == 0 || bitRate == 15 = Nothing
    | otherwise = Just $ MP3FrameHeader bitRate sampleRate
    where
        syncWord = extractBits bs 21 11
        mpegVersion = extractBits bs 19 2
        layerVersion = extractBits bs 17 2
        bitRate = bitRates !! (fromIntegral $ extractBits bs 12 3)
        sampleRate = sampleRates !! (fromIntegral $ extractBits bs 10 2)

mp3Seek :: MP3DataStream -> Maybe (MP3DataStream, MP3FrameHeader)
mp3Seek bs
  | B.length bs < 4 = Nothing
  | otherwise =
      let window = B.take 4 bs
          frame = getMP3FrameHeader $ runGet getWord32be window
       in case frame of
            Just header -> Just (bs, header)
            Nothing     -> mp3Seek (B.drop 1 bs)

main :: IO ()
main = do
    input <- B.readFile "./TastyWaves.mp3"
    let frame = mp3Seek input
    print $ snd <$> frame