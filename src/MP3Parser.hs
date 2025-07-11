module MP3Parser where

import qualified Data.ByteString.Lazy as B
import Data.Binary.Get
import Data.Bits
import Data.Word
import Data.Maybe
import Data.List (unfoldr)
import Debug.Trace

debugShow :: Show a => String -> a -> a
debugShow label x = trace (label ++ ": " ++ show x) x

data MP3DataStream = MP3DataStream {
    mainData :: B.ByteString,
    reservoir :: !B.ByteString -- Used for frame data lookback
} deriving (Show)

data ChannelMode = Stereo | JointStereo | DualChannel | SingleChannel deriving (Show, Eq)

data MP3FrameHeader = MP3FrameHeader {
    bitRate :: Int,
    sampleRate :: Int,
    protection :: Bool,
    channelMode :: ChannelMode,
    padding :: Int
} deriving (Show)

data MP3SideInfo = MP3SideInfo {
    dataOffset :: Int -- Offset for previous frame lookback
} deriving (Show)

data MP3Frame = MP3Frame {
    header :: MP3FrameHeader,
    sideInfo :: MP3SideInfo,
    mp3Data :: MP3DataStream
} deriving (Show)

toBool :: Int -> Bool
toBool 0 = False
toBool _ = True

getChannelMode :: Int -> ChannelMode
getChannelMode 0 = Stereo
getChannelMode 1 = JointStereo
getChannelMode 2 = DualChannel
getChannelMode _ = SingleChannel

extractBits :: (Bits a, Num a) => a -> Int -> Int -> a
extractBits value startIndex size = (value `shiftR` startIndex) .&. ((1 `shiftL` size) - 1)

bitRates :: [Int]
bitRates = map (* 1000) [0, 32, 48, 56, 64, 80, 96, 112, 128, 144, 160, 176, 192, 224, 256, 320, 0]

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
    | otherwise = Just $ MP3FrameHeader bitRate sampleRate protectionOn channelMode framePadding
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
        protectionOn = toBool . fromIntegral $  extractBits bs 16 1

parseSideInfo :: B.ByteString -> MP3SideInfo
parseSideInfo bs = 
    let word = runGet getWord8 bs
    in MP3SideInfo {
        dataOffset = fromIntegral $ extractBits word 0 8
    }

calculateSideInfoSize :: MP3FrameHeader -> Int
calculateSideInfoSize header
    | channel == SingleChannel = 17
    | otherwise = 32
    where channel = channelMode header

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
                fullFrameBytes = B.take frameSize bs
                frameDataBytes = if protection header
                             then B.drop 6 fullFrameBytes
                             else B.drop 4 fullFrameBytes

                sideInfoSize = fromIntegral $ calculateSideInfoSize header
                sideInfoBytes = B.take sideInfoSize frameDataBytes
                sideInfo = parseSideInfo sideInfoBytes
                
                reservoirLength = B.length reservoir
                mainDataOffset = reservoirLength - (fromIntegral $ dataOffset sideInfo)
            
                previousFrameData = if mainDataOffset > 0 
                    then B.drop mainDataOffset reservoir
                    else B.empty
                
                fullMainData = if B.length previousFrameData > 0
                    then previousFrameData `B.append` frameDataBytes
                    else frameDataBytes

                newReservoir = B.take 511 $ B.append reservoir frameDataBytes
                frame = MP3Frame header sideInfo (MP3DataStream fullMainData reservoir)
                
                restFrames = B.drop frameSize bs
                newDataStream = MP3DataStream restFrames newReservoir

            in Just (frame, newDataStream)
        Nothing -> mp3Seek $ MP3DataStream (B.drop 1 bs) B.empty
    where maybeHeader = getMP3FrameHeader $ runGet getWord32be (B.take 4 bs)


parseAllFrames :: MP3DataStream -> [MP3Frame]
parseAllFrames = unfoldr mp3Seek

main :: IO ()
main = do
    input <- B.readFile "./TastyWaves.mp3"
    let frame = parseAllFrames $ MP3DataStream input B.empty
    print $ frame