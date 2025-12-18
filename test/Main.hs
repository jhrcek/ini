{-# LANGUAGE OverloadedStrings #-}

-- | Simple test suite.

module Main where

import qualified Data.HashMap.Strict as HM
import           Data.Ini
import           Test.Hspec

main :: IO ()
main =
    hspec $ do
        describe "Regular files" $ do
            it "Multi-section file with comments" $
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

            it "File with globals" $
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

            it "File with invalid keys" $
                parseIni
                    "Name=Foo\n\
                    \Name[en_GB]=Fubar"
                    `shouldBe` Left "Failed reading: Name[en_GB]=Fubar"

            it "File ending with commented out sections" $
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
