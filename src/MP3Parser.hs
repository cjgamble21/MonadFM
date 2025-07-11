module MP3Parser where

import qualified Data.ByteString.Lazy as B
import Data.Binary.Get
import Data.Bits
import Data.Word
import Data.Maybe
import Data.List (unfoldr)

data MP3DataStream = MP3DataStream {
    mainData :: B.ByteString,
    reservoir :: !B.ByteString -- Used for frame data lookback
} deriving (Show)

data ChannelMode = Stereo | JointStereo | DualChannel | SingleChannel deriving (Show)

data MP3FrameHeader = MP3FrameHeader {
    bitRate :: Int,
    sampleRate :: Int,
    channelMode :: ChannelMode,
    padding :: Int
} deriving (Show)

data MP3Frame = MP3Frame {
    header :: MP3FrameHeader,
    mp3Data :: MP3DataStream
} deriving (Show)

getChannelMode :: Int -> ChannelMode
getChannelMode 0 = Stereo
getChannelMode 1 = JointStereo
getChannelMode 2 = DualChannel
getChannelMode _ = SingleChannel

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
    | otherwise = Just $ MP3FrameHeader bitRate sampleRate channelMode framePadding
    where
        syncWord = extractBits bs 21 11
        mpegVersion = extractBits bs 19 2
        layerVersion = extractBits bs 17 2
        bitRateIndex = fromIntegral $ extractBits bs 12 4
        bitRate = bitRates !! bitRateIndex
        sampleRateIndex = fromIntegral $ extractBits bs 10 2
        sampleRate = sampleRates !! sampleRateIndex
        framePadding = fromIntegral $ extractBits bs 9 1
        channelMode = getChannelMode . fromIntegral $ extractBits bs 6 2

calculateFrameSize :: Num a =>  MP3FrameHeader -> a
calculateFrameSize frame =
    fromIntegral $ 144 * (bitRate frame) `div` (sampleRate frame) + (padding frame)

mp3Seek :: MP3DataStream -> Maybe (MP3Frame, MP3DataStream)
mp3Seek (MP3DataStream bs reservoir)
  | B.length bs < 4 = Nothing
  | otherwise = 
    case maybeHeader of
        Just header -> 
            let frameSize = calculateFrameSize header
                frameData = B.take frameSize bs
            in Just (MP3Frame header (MP3DataStream frameData B.empty), MP3DataStream (B.drop frameSize bs) B.empty)
        Nothing -> mp3Seek $ MP3DataStream (B.drop 1 bs) B.empty
    where maybeHeader = getMP3FrameHeader $ runGet getWord32be (B.take 4 bs)


parseAllFrames :: MP3DataStream -> [MP3Frame]
parseAllFrames = unfoldr mp3Seek

main :: IO ()
main = do
    input <- B.readFile "./1-second-of-silence.mp3"
    let frame = parseAllFrames $ MP3DataStream input B.empty
    print $ frame