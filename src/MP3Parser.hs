module MP3Parser where

import qualified Data.ByteString.Lazy as B
import Data.Binary.Get
import Data.Bits
import Data.Word
import Data.Maybe
import Data.List (unfoldr)

type MP3DataStream = B.ByteString

data MP3FrameHeader = MP3FrameHeader {
    bitRate :: Int,
    sampleRate :: Int,
    padding :: Int
} deriving (Show)

extractBits :: (Bits a, Num a) => a -> Int -> Int -> a
extractBits value startIndex size = (value `shiftR` startIndex) .&. ((1 `shiftL` size) - 1)

bitRates :: [Int]
bitRates = [0, 32, 48, 56, 64, 80, 96, 112, 128, 144, 160, 176, 192, 224, 256, 320, 0]

sampleRates :: [Int]
sampleRates = [44100, 48000, 32000]

getMP3FrameHeader :: Word32 -> Maybe MP3FrameHeader
getMP3FrameHeader bs
    | syncWord /= 0x7FF = Nothing
    | mpegVersion /= 3 = Nothing -- Only supporting MPEG-1
    | layerVersion /= 1 = Nothing -- Only supporting Layer 3
    | bitRateIndex == 0 || bitRateIndex == 15 = Nothing
    | sampleRateIndex < 0 || sampleRateIndex > 2 = Nothing
    | framePadding > 1 = Nothing
    | otherwise = Just $ MP3FrameHeader bitRate sampleRate framePadding
    where
        syncWord = extractBits bs 21 11
        mpegVersion = extractBits bs 19 2
        layerVersion = extractBits bs 17 2
        bitRateIndex = fromIntegral $ extractBits bs 12 4
        bitRate = bitRates !! bitRateIndex
        sampleRateIndex = fromIntegral $ extractBits bs 10 2
        sampleRate = sampleRates !! sampleRateIndex
        framePadding = fromIntegral $ extractBits bs 9 1

calculateFrameSize :: Num a =>  MP3FrameHeader -> a
calculateFrameSize frame =
    fromIntegral $ 144 * (bitRate frame) `div` (sampleRate frame) + (padding frame)

mp3Seek :: MP3DataStream -> Maybe (MP3FrameHeader, MP3DataStream)
mp3Seek bs
  | B.length bs < 4 = Nothing
  | otherwise = 
    case header of
        Just frame -> Just (frame, B.drop (calculateFrameSize frame) bs)
        Nothing -> mp3Seek (B.drop 1 bs)
    where header = getMP3FrameHeader $ runGet getWord32be (B.take 4 bs)


parseAllFrames :: MP3DataStream -> [MP3FrameHeader]
parseAllFrames = unfoldr mp3Seek

main :: IO ()
main = do
    input <- B.readFile "./TastyWaves.mp3"
    let frame = parseAllFrames input
    print $ frame