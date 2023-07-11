-- Copyright (C) 2012 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module Main
	( tests
	, main
	) where

import           Test.Chell (Suite, defaultMain)

import           OptionsTests.Api (suite_Api)
import           OptionsTests.Help (suite_Help)
import           OptionsTests.OptionTypes (suite_OptionTypes)
import           OptionsTests.StringParsing (suite_StringParsing)
import           OptionsTests.Tokenize (suite_Tokenize)
import           OptionsTests.Util (suite_Util)

tests :: [Suite]
tests =
	[ suite_Api
	, suite_Help
	, suite_OptionTypes
	, suite_StringParsing
	, suite_Tokenize
	, suite_Util
	]

main :: IO ()
main = Test.Chell.defaultMain tests
