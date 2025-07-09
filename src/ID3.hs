import qualified Data.ByteString as BS
import Data.Binary.Get
import Data.Word

data ID3Header = ID3Header {
    version :: Word16,
    flags :: Word8,
    size :: Int
} deriving (Show)

parseID3Header :: Get ID3Header
parseID3Header = ID3Header <$ getByteString 3 <*> getWord16le <*> getWord8 <*> getInthost

main :: IO ()
main = BS.readFile "./1-second-of-silence.mp3" >>=
    print . runGet parseID3Header . BS.fromStrict
