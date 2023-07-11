{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2012 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module OptionsTests.OptionTypes
	( suite_OptionTypes
	) where

import           Data.Int
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Word

import           Test.Chell

import           Options

suite_OptionTypes :: Suite
suite_OptionTypes = suite "option-types"
	[ test_Bool
	, test_String
	, test_Int
	, test_Int8
	, test_Int16
	, test_Int32
	, test_Int64
	, test_Word
	, test_Word8
	, test_Word16
	, test_Word32
	, test_Word64
	, test_Integer
	, test_Float
	, test_Double
	, test_Maybe
	, test_List
	, test_Set
	, test_Map
	, test_Enum
	]

parseValid :: (Show a, Eq a) => OptionType a -> String -> a -> Assertion
parseValid t s expected = equal (optionTypeParse t s) (Right expected)

parseInvalid :: (Show a, Eq a) => OptionType a -> String -> String -> Assertion
parseInvalid t s err = equal (optionTypeParse t s) (Left err)

test_Bool :: Test
test_Bool = assertions "bool" $ do
	$expect (parseValid optionType_bool "true" True)
	$expect (parseValid optionType_bool "false" False)
	$expect (parseInvalid optionType_bool "" "\"\" is not in {\"true\", \"false\"}.")

test_String :: Test
test_String = assertions "string" $ do
	let valid = parseValid optionType_string
	let invalid = parseInvalid optionType_string
	
	$expect (valid "" "")
	$expect (valid "a" "a")
	$expect (valid "\12354" "\12354")
	$expect (valid "\56507" "\56507")
	$expect (valid "\61371" "\61371")

test_Int :: Test
test_Int = assertions "int" $ do
	let valid = parseValid optionType_int
	let invalid = parseInvalid optionType_int
	
	$expect (valid "-1" (-1 :: Int))
	$expect (valid "1" (1 :: Int))
	$expect (invalid "a" "\"a\" is not an integer.")
	
	let pastMin = show (toInteger (minBound :: Int) - 1)
	let pastMax = show (toInteger (maxBound :: Int) + 1)
	let errBounds = " is not within bounds [" ++ show (minBound :: Int) ++ ":" ++ show (maxBound :: Int) ++ "] of type int."
	
	$expect (invalid pastMin (pastMin ++ errBounds))
	$expect (valid (show (minBound :: Int)) minBound)
	$expect (valid (show (maxBound :: Int)) maxBound)
	$expect (invalid pastMax (pastMax ++ errBounds))

test_Int8 :: Test
test_Int8 = assertions "int8" $ do
	let valid = parseValid optionType_int8
	let invalid = parseInvalid optionType_int8
	
	$expect (valid "-1" (-1 :: Int8))
	$expect (valid "1" (1 :: Int8))
	$expect (invalid "a" "\"a\" is not an integer.")
	
	let pastMin = show (toInteger (minBound :: Int8) - 1)
	let pastMax = show (toInteger (maxBound :: Int8) + 1)
	$expect (invalid pastMin "-129 is not within bounds [-128:127] of type int8.")
	$expect (valid (show (minBound :: Int8)) minBound)
	$expect (valid (show (maxBound :: Int8)) maxBound)
	$expect (invalid pastMax "128 is not within bounds [-128:127] of type int8.")

test_Int16 :: Test
test_Int16 = assertions "int16" $ do
	let valid = parseValid optionType_int16
	let invalid = parseInvalid optionType_int16
	
	$expect (valid "-1" (-1 :: Int16))
	$expect (valid "1" (1 :: Int16))
	$expect (invalid "a" "\"a\" is not an integer.")
	
	let pastMin = show (toInteger (minBound :: Int16) - 1)
	let pastMax = show (toInteger (maxBound :: Int16) + 1)
	$expect (invalid pastMin "-32769 is not within bounds [-32768:32767] of type int16.")
	$expect (valid (show (minBound :: Int16)) minBound)
	$expect (valid (show (maxBound :: Int16)) maxBound)
	$expect (invalid pastMax "32768 is not within bounds [-32768:32767] of type int16.")

test_Int32 :: Test
test_Int32 = assertions "int32" $ do
	let valid = parseValid optionType_int32
	let invalid = parseInvalid optionType_int32
	
	$expect (valid "-1" (-1 :: Int32))
	$expect (valid "1" (1 :: Int32))
	$expect (invalid "a" "\"a\" is not an integer.")
	
	let pastMin = show (toInteger (minBound :: Int32) - 1)
	let pastMax = show (toInteger (maxBound :: Int32) + 1)
	$expect (invalid pastMin "-2147483649 is not within bounds [-2147483648:2147483647] of type int32.")
	$expect (valid (show (minBound :: Int32)) minBound)
	$expect (valid (show (maxBound :: Int32)) maxBound)
	$expect (invalid pastMax "2147483648 is not within bounds [-2147483648:2147483647] of type int32.")

test_Int64 :: Test
test_Int64 = assertions "int64" $ do
	let valid = parseValid optionType_int64
	let invalid = parseInvalid optionType_int64
	
	$expect (valid "-1" (-1 :: Int64))
	$expect (valid "1" (1 :: Int64))
	$expect (invalid "a" "\"a\" is not an integer.")
	
	let pastMin = show (toInteger (minBound :: Int64) - 1)
	let pastMax = show (toInteger (maxBound :: Int64) + 1)
	$expect (invalid pastMin "-9223372036854775809 is not within bounds [-9223372036854775808:9223372036854775807] of type int64.")
	$expect (valid (show (minBound :: Int64)) minBound)
	$expect (valid (show (maxBound :: Int64)) maxBound)
	$expect (invalid pastMax "9223372036854775808 is not within bounds [-9223372036854775808:9223372036854775807] of type int64.")

test_Word :: Test
test_Word = assertions "word" $ do
	let valid = parseValid optionType_word
	let invalid = parseInvalid optionType_word
	
	let pastMax = show (toInteger (maxBound :: Word) + 1)
	let errBounds = " is not within bounds [0:" ++ show (maxBound :: Word) ++ "] of type uint."
	
	$expect (invalid "-1" ("-1" ++ errBounds))
	$expect (valid "0" (0 :: Word))
	$expect (valid "1" (1 :: Word))
	$expect (invalid "a" "\"a\" is not an integer.")
	
	$expect (valid (show (maxBound :: Word)) maxBound)
	$expect (invalid pastMax (pastMax ++ errBounds))

test_Word8 :: Test
test_Word8 = assertions "word8" $ do
	let valid = parseValid optionType_word8
	let invalid = parseInvalid optionType_word8
	
	$expect (invalid "-1" "-1 is not within bounds [0:255] of type uint8.")
	$expect (valid "0" (0 :: Word8))
	$expect (valid "1" (1 :: Word8))
	$expect (invalid "a" "\"a\" is not an integer.")
	
	let pastMax = show (toInteger (maxBound :: Word8) + 1)
	$expect (valid (show (maxBound :: Word8)) maxBound)
	$expect (invalid pastMax "256 is not within bounds [0:255] of type uint8.")

test_Word16 :: Test
test_Word16 = assertions "word16" $ do
	let valid = parseValid optionType_word16
	let invalid = parseInvalid optionType_word16
	
	$expect (invalid "-1" "-1 is not within bounds [0:65535] of type uint16.")
	$expect (valid "0" (0 :: Word16))
	$expect (valid "1" (1 :: Word16))
	$expect (invalid "a" "\"a\" is not an integer.")
	
	let pastMax = show (toInteger (maxBound :: Word16) + 1)
	$expect (valid (show (maxBound :: Word16)) maxBound)
	$expect (invalid pastMax "65536 is not within bounds [0:65535] of type uint16.")

test_Word32 :: Test
test_Word32 = assertions "word32" $ do
	let valid = parseValid optionType_word32
	let invalid = parseInvalid optionType_word32
	
	$expect (invalid "-1" "-1 is not within bounds [0:4294967295] of type uint32.")
	$expect (valid "0" (0 :: Word32))
	$expect (valid "1" (1 :: Word32))
	$expect (invalid "a" "\"a\" is not an integer.")
	
	let pastMax = show (toInteger (maxBound :: Word32) + 1)
	$expect (valid (show (maxBound :: Word32)) maxBound)
	$expect (invalid pastMax "4294967296 is not within bounds [0:4294967295] of type uint32.")

test_Word64 :: Test
test_Word64 = assertions "word64" $ do
	let valid = parseValid optionType_word64
	let invalid = parseInvalid optionType_word64
	
	$expect (invalid "-1" "-1 is not within bounds [0:18446744073709551615] of type uint64.")
	$expect (valid "0" (0 :: Word64))
	$expect (valid "1" (1 :: Word64))
	$expect (invalid "a" "\"a\" is not an integer.")
	
	let pastMax = show (toInteger (maxBound :: Word64) + 1)
	$expect (valid (show (maxBound :: Word64)) maxBound)
	$expect (invalid pastMax "18446744073709551616 is not within bounds [0:18446744073709551615] of type uint64.")

test_Integer :: Test
test_Integer = assertions "integer" $ do
	let valid = parseValid optionType_integer
	let invalid = parseInvalid optionType_integer
	
	$expect (invalid "" "\"\" is not an integer.")
	$expect (valid "-1" (-1 :: Integer))
	$expect (valid "0" (0 :: Integer))
	$expect (valid "1" (1 :: Integer))
	$expect (invalid "a" "\"a\" is not an integer.")

test_Float :: Test
test_Float = assertions "float" $ do
	let valid = parseValid optionType_float
	let invalid = parseInvalid optionType_float
	
	$expect (valid "-1" (-1 :: Float))
	$expect (valid "0" (0 :: Float))
	$expect (valid "1" (1 :: Float))
	$expect (valid "1.5" (1.5 :: Float))
	$expect (valid "3e5" (3e5 :: Float))
	$expect (invalid "a" "\"a\" is not a number.")

test_Double :: Test
test_Double = assertions "double" $ do
	let valid = parseValid optionType_double
	let invalid = parseInvalid optionType_double
	
	$expect (valid "-1" (-1 :: Double))
	$expect (valid "0" (0 :: Double))
	$expect (valid "1" (1 :: Double))
	$expect (valid "1.5" (1.5 :: Double))
	$expect (valid "3e5" (3e5 :: Double))
	$expect (invalid "a" "\"a\" is not a number.")

test_Maybe :: Test
test_Maybe = assertions "maybe" $ do
	let t = optionType_maybe optionType_int
	let valid = parseValid t
	let invalid = parseInvalid t
	
	$expect (valid "" Nothing)
	$expect (valid "1" (Just 1))
	$expect (invalid "a" "\"a\" is not an integer.")

test_List :: Test
test_List = assertions "list" $ do
	let t = optionType_list ',' optionType_int
	let valid = parseValid t
	let invalid = parseInvalid t
	
	$expect (valid "" [])
	$expect (valid "1" [1])
	$expect (valid "1,2,3" [1, 2, 3])
	$expect (valid "1,1,2,3" [1, 1, 2, 3])
	$expect (invalid "1,a,3" "\"a\" is not an integer.")

test_Set :: Test
test_Set = assertions "set" $ do
	let t = optionType_set ',' optionType_int
	let valid = parseValid t
	let invalid = parseInvalid t
	
	$expect (valid "" Set.empty)
	$expect (valid "1" (Set.fromList [1]))
	$expect (valid "1,2,3" (Set.fromList [1, 2, 3]))
	$expect (valid "1,1,2,3" (Set.fromList [1, 2, 3]))
	$expect (invalid "1,a,3" "\"a\" is not an integer.")

test_Map :: Test
test_Map = assertions "map" $ do
	let t = optionType_map ',' '=' optionType_int optionType_int
	let valid = parseValid t
	let invalid = parseInvalid t
	
	$expect (valid "" Map.empty)
	$expect (valid "1=100" (Map.fromList [(1, 100)]))
	$expect (valid "1=100,2=200,3=300" (Map.fromList [(1, 100), (2, 200), (3, 300)]))
	$expect (valid "1=100,2=200,1=300" (Map.fromList [(1, 300), (2, 200)]))
	$expect (invalid "a=1" "\"a\" is not an integer.")
	$expect (invalid "1=a" "\"a\" is not an integer.")
	$expect (invalid "1=" "\"\" is not an integer.")
	$expect (invalid "1" "Map item \"1\" has no value.")

data TestEnum = Enum1 | Enum2 | Enum3
	deriving (Bounded, Enum, Eq, Show)

test_Enum :: Test
test_Enum = assertions "enum" $ do
	let t = optionType_enum "test enum"
	let valid = parseValid t
	let invalid = parseInvalid t
	
	$expect (valid "Enum1" Enum1)
	$expect (valid "Enum2" Enum2)
	$expect (invalid "Enum4" "\"Enum4\" is not in {\"Enum1\", \"Enum2\", \"Enum3\"}.")
