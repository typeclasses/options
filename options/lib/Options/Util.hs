{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module: Options.Util
-- License: MIT
module Options.Util where

import           Data.Char (isAlphaNum, isLetter, isUpper)
import qualified Data.Set as Set

#if defined(OPTIONS_ENCODING_UTF8)
import           Data.Char (chr)
import           Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import qualified Data.ByteString.Char8 as Char8
import           Foreign
import           Foreign.C
#endif

stringToGhc704 :: String -> String
#if defined(OPTIONS_ENCODING_UTF8)
stringToGhc704 = decodeUtf8 . Char8.pack

decodeUtf8 :: Char8.ByteString -> String
decodeUtf8 bytes = map (chr . fromIntegral) word32s where
	word32s = unsafePerformIO (unsafeUseAsCStringLen bytes io)
	io (bytesPtr, len) = allocaArray len $ \wordsPtr -> do
		nWords <- c_decodeString (castPtr bytesPtr) wordsPtr (fromIntegral len)
		peekArray (fromIntegral nWords) wordsPtr

foreign import ccall unsafe "hsoptions_decode_string"
	c_decodeString :: Ptr Word8 -> Ptr Word32 -> CInt -> IO CInt
#else
stringToGhc704 = id
#endif

validFieldName :: String -> Bool
validFieldName = valid where
	valid s = case s of
		[] -> False
		c : cs -> validFirst c && all validGeneral cs
	validFirst c = c == '_' || (isLetter c && not (isUpper c))
	validGeneral c = isAlphaNum c || c == '_' || c == '\''

validShortFlag :: Char -> Bool
validShortFlag = isAlphaNum

validLongFlag :: String -> Bool
validLongFlag = valid where
	valid s = case s of
		[] -> False
		c : cs -> validFirst c && all validGeneral cs
	validFirst = isAlphaNum
	validGeneral c = isAlphaNum c || c == '-' || c == '_'

hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates xs = Set.size (Set.fromList xs) /= length xs

mapEither :: (a -> Either err b) -> [a] -> Either err [b]
mapEither fn = loop [] where
	loop acc [] = Right (reverse acc)
	loop acc (a:as) = case fn a of
		Left err -> Left err
		Right b -> loop (b:acc) as
