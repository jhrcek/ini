{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Simple test suite.

module Main where

import           Data.Char (isControl, isSpace)
import qualified Data.HashMap.Strict as HM
import           Data.Ini (Ini (..), KeySeparator (..), WriteIniSettings (..), parseIni, printIniWith)
import           Data.Text (Text)
import qualified Data.Text as T
import           Test.Hspec (describe, hspec, it, shouldBe)
import           Test.QuickCheck

main :: IO ()
main =
    hspec $ do
        describe "parseIni" $ do
            it "parses multi-section file with comments" $
                parseIni
                    "# Some comment.\n\
                    \[SERVER]\n\
                    \port=6667\n\
                    \hostname=localhost\n\
                    \[AUTH]\n\
                    \user=hello\n\
                    \pass=world\n\
                    \# Salt can be an empty string.\n\
                    \salt="
                    `shouldBe` Right
                        ( Ini
                            { iniSections =
                                HM.fromList
                                    [
                                        ( "AUTH"
                                        ,
                                            [ ("user", "hello")
                                            , ("pass", "world")
                                            , ("salt", "")
                                            ]
                                        )
                                    ,
                                        ( "SERVER"
                                        , [("port", "6667"), ("hostname", "localhost")]
                                        )
                                    ]
                            , iniGlobals = []
                            }
                        )

            it "parses file with globals" $
                parseIni
                    "# Some comment.\n\
                    \port=6667\n\
                    \hostname=localhost\n\
                    \[AUTH]\n\
                    \user=hello\n\
                    \pass=world\n\
                    \# Salt can be an empty string.\n\
                    \salt="
                    `shouldBe` Right
                        ( Ini
                            { iniSections =
                                HM.fromList
                                    [
                                        ( "AUTH"
                                        ,
                                            [ ("user", "hello")
                                            , ("pass", "world")
                                            , ("salt", "")
                                            ]
                                        )
                                    ]
                            , iniGlobals =
                                [("port", "6667"), ("hostname", "localhost")]
                            }
                        )

            it "fails to parse file with invalid keys" $
                parseIni
                    "Name=Foo\n\
                    \Name[en_GB]=Fubar"
                    `shouldBe` Left "Failed reading: Name[en_GB]=Fubar"

            it "parses file ending with comments" $
                parseIni
                    "[default]\n\
                    \a = 1\n\
                    \\n\
                    \#[staging-PI]\n\
                    \#a = 2\n"
                    `shouldBe` Right
                        ( Ini
                            { iniSections = HM.fromList [("default", [("a", "1")])]
                            , iniGlobals = []
                            }
                        )

            it "parses file with globals only" $
                parseIni
                    "global1 = hello\n\
                    \global2 = 123\n\
                    \# An end of file comment here"
                    `shouldBe` Right
                        ( Ini
                            { iniSections = HM.empty
                            , iniGlobals = [("global1", "hello"), ("global2", "123")]
                            }
                        )

            it "parses empty file" $
                parseIni "" `shouldBe` Right mempty

            it "roundtrips with printIniWith" $
                property $ \ini settings ->
                    let printed = printIniWith settings ini
                        parsed = parseIni printed
                     in counterexample (T.unpack printed) $
                            parsed === Right ini

genKey :: Gen Text
genKey = do
    firstChar <- arbitrary `suchThat` isValidFirstKeyChar
    restChars <- listOf $ arbitrary `suchThat` isValidKeyChar
    pure $ T.pack (firstChar : restChars)
  where
    isValidKeyChar c = not (c == '=' || c == ':' || c == '[' || c == ']' || isControl c || isSpace c)
    isValidFirstKeyChar c = isValidKeyChar c && c /= ';' && c /= '#'

genSectionName :: Gen Text
genSectionName = do
    chars <- listOf1 $ arbitrary `suchThat` isValidSectionChar
    let name = T.strip $ T.pack chars
    if T.null name
        then genSectionName -- retry if stripping results in empty
        else pure name
  where
    isValidSectionChar c = c /= '[' && c /= ']' && not (isControl c)

genValue :: Gen Text
genValue = do
    chars <- listOf $ arbitrary `suchThat` (not . isControl)
    pure $ T.strip $ T.pack chars

genKeyValue :: Gen (Text, Text)
genKeyValue = (,) <$> genKey <*> genValue

instance Arbitrary KeySeparator where
    arbitrary = arbitraryBoundedEnum
    shrink ColonKeySeparator = []
    shrink EqualsKeySeparator = [ColonKeySeparator]

instance Arbitrary Ini where
    arbitrary = do
        numSections <- choose (0, 2)
        sections <- vectorOf numSections $ do
            name <- genSectionName
            numPairs <- choose (0, 2)
            pairs <- vectorOf numPairs genKeyValue
            pure (name, pairs)
        numGlobals <- choose (0, 3)
        globals <- vectorOf numGlobals genKeyValue
        pure $
            Ini
                { iniSections = HM.fromList sections
                , iniGlobals = globals
                }
    shrink ini =
        -- Shrink by removing sections
        [ ini{iniSections = HM.fromList secs'}
        | secs' <- shrinkList (const []) (HM.toList (iniSections ini))
        ]
            ++
            -- Shrink by removing key-value pairs from sections
            [ ini{iniSections = HM.fromList secs'}
            | secs' <- shrinkOne shrinkSection (HM.toList (iniSections ini))
            ]
            ++
            -- Shrink by removing globals
            [ ini{iniGlobals = globals'}
            | globals' <- shrinkList (const []) (iniGlobals ini)
            ]
      where
        shrinkSection (name, pairs) = [(name, pairs') | pairs' <- shrinkList (const []) pairs]
        shrinkOne _ [] = []
        shrinkOne f (x : xs) = [x' : xs | x' <- f x] ++ [x : xs' | xs' <- shrinkOne f xs]

instance Arbitrary WriteIniSettings where
    arbitrary = WriteIniSettings <$> arbitrary
    shrink (WriteIniSettings sep) = WriteIniSettings <$> shrink sep
