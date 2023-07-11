{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2014 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module OptionsTests.Api
	( suite_Api
	) where

import           Control.Applicative
import           Test.Chell

import           Options

suite_Api :: Suite
suite_Api = suite "api"
	[ test_RepeatedFlags
	, test_CompatibleDuplicateFlags
	, test_ConflictingDuplicateFlags
	]

data RepeatedStringOpts = RepeatedStringOpts [String]
	deriving (Eq, Show)

repeatedStringList :: OptionType [String]
repeatedStringList = (optionType "repeated-string-list" [] (\x -> Right [x]) show)
	{ optionTypeMerge = Just concat
	}

instance Options RepeatedStringOpts where
	defineOptions = pure RepeatedStringOpts
		<*> defineOption repeatedStringList (\o -> o
			{ optionShortFlags = ['s']
			})

test_RepeatedFlags :: Test
test_RepeatedFlags = assertions "repeated-flags" $ do
	let parsed = parseOptions ["-sfoo", "-sbar", "-sbaz"]
	$expect (nothing (parsedError parsed))
	$expect (equal (parsedOptions parsed) (Just (RepeatedStringOpts ["foo", "bar", "baz"])))

data CompatibleDuplicateOpts = CompatibleDuplicateOpts SubOpts1 SubOpts1
	deriving (Eq, Show)

instance Options CompatibleDuplicateOpts where
	defineOptions = pure CompatibleDuplicateOpts
		<*> defineOptions
		<*> defineOptions

data ConflictingDuplicateOpts = ConflictingDuplicateOpts SubOpts1 SubOpts2
	deriving (Eq, Show)

instance Options ConflictingDuplicateOpts where
	defineOptions = pure ConflictingDuplicateOpts
		<*> defineOptions
		<*> defineOptions

data SubOpts1 = SubOpts1 Integer
	deriving (Eq, Show)

data SubOpts2 = SubOpts2 Integer
	deriving (Eq, Show)

instance Options SubOpts1 where
	defineOptions = pure SubOpts1
		<*> simpleOption "int" 0 ""

instance Options SubOpts2 where
	defineOptions = pure SubOpts2
		<*> simpleOption "int" 1 ""

test_CompatibleDuplicateFlags :: Test
test_CompatibleDuplicateFlags = assertions "compatible-duplicate-flags" $ do
	let parsed = parseOptions ["--int=10"]
	$expect (nothing (parsedError parsed))
	$expect (equal (parsedOptions parsed) (Just (CompatibleDuplicateOpts (SubOpts1 10) (SubOpts1 10))))

test_ConflictingDuplicateFlags :: Test
test_ConflictingDuplicateFlags = assertions "conflicting-duplicate-flags" $ do
	let parsed = parseOptions ["-sfoo", "-sbar", "-sbaz"]
	$expect (equal (parsedError parsed) (Just "Duplicate option flag \"--int\"."))
	$expect (nothing (parsedOptions parsed :: Maybe ConflictingDuplicateOpts))
