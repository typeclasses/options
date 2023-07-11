{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2012 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module OptionsTests.Util
	( suite_Util
	) where

#if defined(OPTIONS_ENCODING_UTF8)
import           Data.Bits
import qualified Data.ByteString.Char8 as Char8
import           Data.Char (chr, ord)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Test.Chell.QuickCheck
import           Test.QuickCheck (Property, forAll)
import           Test.QuickCheck.Gen
#endif

import           Test.Chell

import           Options.Util

suite_Util :: Suite
suite_Util = suite "util"
	[ test_ValidFieldName
	, test_ValidShortFlag
	, test_ValidLongFlag
	, test_HasDuplicates
#if defined(OPTIONS_ENCODING_UTF8)
	, property "decodeUtf8" prop_DecodeUtf8
#endif
	]

test_ValidFieldName :: Test
test_ValidFieldName = assertions "validFieldName" $ do
	$expect (validFieldName "a")
	$expect (validFieldName "abc")
	$expect (validFieldName "_abc_")
	$expect (validFieldName "abc'")
	$expect (validFieldName "\12354")
	$expect (not (validFieldName ""))
	$expect (not (validFieldName "'a"))
	$expect (not (validFieldName "a b"))
	$expect (not (validFieldName "Ab"))

test_ValidShortFlag :: Test
test_ValidShortFlag = assertions "validShortFlag" $ do
	$expect (validShortFlag 'a')
	$expect (validShortFlag 'A')
	$expect (validShortFlag '0')
	$expect (validShortFlag '\12354')
	$expect (not (validShortFlag ' '))
	$expect (not (validShortFlag '-'))

test_ValidLongFlag :: Test
test_ValidLongFlag = assertions "validLongFlag" $ do
	$expect (validLongFlag "a")
	$expect (validLongFlag "A")
	$expect (validLongFlag "abc")
	$expect (validLongFlag "0")
	$expect (validLongFlag "012")
	$expect (validLongFlag "a-b")
	$expect (validLongFlag "a_b")
	$expect (validLongFlag "\12354bc")
	$expect (not (validLongFlag ""))
	$expect (not (validLongFlag "a b"))
	$expect (not (validLongFlag "a+b"))
	$expect (not (validLongFlag "-"))
	$expect (not (validLongFlag "--"))

test_HasDuplicates :: Test
test_HasDuplicates = assertions "hasDuplicates" $ do
	$expect (not (hasDuplicates ([] :: [Char])))
	$expect (not (hasDuplicates ['a', 'b']))
	$expect (hasDuplicates ['a', 'b', 'a'])

#if defined(OPTIONS_ENCODING_UTF8)
prop_DecodeUtf8 :: Property
prop_DecodeUtf8 = forAll example prop where
	example = do
		chunks <- listOf genChunk
		let utf = concat [x | (x, _) <- chunks]
		let chars = concat [x | (_, x) <- chunks]
		return (Char8.pack utf, chars)
	genChunk = do
		unichr <- genUnichar
		let utf = Char8.unpack (Text.encodeUtf8 (Text.singleton unichr))
		nBytes <- choose (1, length utf)
		let truncUtf = take nBytes utf
		return $ if nBytes == length utf
			then (utf, [unichr])
			else (truncUtf, map (\c -> chr (ord c + 0xDC00)) truncUtf)
	prop (bytes, expected) = decodeUtf8 bytes == expected

genUnichar :: Gen Char
genUnichar = chr `fmap` excluding reserved (oneof planes) where
	excluding :: [a -> Bool] -> Gen a -> Gen a
	excluding bad gen = loop where
		loop = do
			x <- gen
			if or (map ($ x) bad)
				then loop
				else return x
	
	reserved = [lowSurrogate, highSurrogate, noncharacter]
	lowSurrogate c = c >= 0xDC00 && c <= 0xDFFF
	highSurrogate c = c >= 0xD800 && c <= 0xDBFF
	noncharacter c = masked == 0xFFFE || masked == 0xFFFF where
		masked = c .&. 0xFFFF
	
	ascii = choose (0,0x7F)
	plane0 = choose (0xF0, 0xFFFF)
	plane1 = oneof [ choose (0x10000, 0x10FFF)
	               , choose (0x11000, 0x11FFF)
	               , choose (0x12000, 0x12FFF)
	               , choose (0x13000, 0x13FFF)
	               , choose (0x1D000, 0x1DFFF)
	               , choose (0x1F000, 0x1FFFF)
	               ]
	plane2 = oneof [ choose (0x20000, 0x20FFF)
	               , choose (0x21000, 0x21FFF)
	               , choose (0x22000, 0x22FFF)
	               , choose (0x23000, 0x23FFF)
	               , choose (0x24000, 0x24FFF)
	               , choose (0x25000, 0x25FFF)
	               , choose (0x26000, 0x26FFF)
	               , choose (0x27000, 0x27FFF)
	               , choose (0x28000, 0x28FFF)
	               , choose (0x29000, 0x29FFF)
	               , choose (0x2A000, 0x2AFFF)
	               , choose (0x2B000, 0x2BFFF)
	               , choose (0x2F000, 0x2FFFF)
	               ]
	plane14 = choose (0xE0000, 0xE0FFF)
	planes = [ascii, plane0, plane1, plane2, plane14]
#endif
