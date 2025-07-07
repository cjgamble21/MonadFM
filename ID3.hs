import qualified Data.ByteString as BS
import Data.Binary.Get
import Data.Word

data ID3Header = ID3Header {
    version :: Word16,
    flags :: Word8,
    size :: Int
} deriving (Show)

parseID3Header :: Get ID3Header
parseID3Header = do
    _ <- getByteString 3
    version <- getWord16le
    flags <- getWord8
    size <- getInthost

    return $ ID3Header version flags size

main :: IO ()
main = BS.readFile "TastyWaves.mp3" >>=
    print . runGet parseID3Header . BS.fromStrict
