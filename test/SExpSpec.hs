module SExpSpec (spec) where

import RIO hiding (bool)
import SExp
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Megaparsec

spec :: Spec
spec = do
    describe "atom parser" do
        it "parses true" do
            parse atom "" "true" `shouldParse` SBool True
        it "parses 42" do
            parse atom "" "42" `shouldParse` SInteger 42
        it "parses an identifier" do
            parse atom "" "foo" `shouldParse` SId (Identifier{id = "foo"})
        prop "parses all alphabetic identifiers" do
            let genAlphaNum = elements (['a' .. 'z'] ++ ['A' .. 'Z'])
            let nonEmptyString = listOf1 genAlphaNum
            forAll nonEmptyString (\str -> parse atom "" str == Right (SId (Identifier{id = str})))
        it "parses an identifier with digits" do
            parse atom "" "bar42spam" `shouldParse` SId (Identifier{id = "bar42spam"})
        it "parse empty string" do
            parse atom "" "\"\"" `shouldParse` SString ""
        it "parse foo" do
            parse atom "" "\"foo\"" `shouldParse` SString "foo"
        it "parses newline" do
            parse atom "" ['"', '\n', '"'] `shouldParse` SString "\n"
        it "parses string starting with unicode non latin char" do
            parse atom "" "\"衄\"" `shouldParse` SString "衄"
        it "parses double quote" do
            parse atom "" "\"Hello \\\"world\\\"\"" `shouldParse` SString "Hello \"world\""
        it "parses an s expression" do
            parse atom "" "(\"hello\" 1)" `shouldParse` SSExp [SString "hello", SInteger 1]
        it "parses a nested s expression" do
            parse atom "" "(\"hello\" \"world\" foo (1 2 spam))" `shouldParse` SSExp [SString "hello", SString "world", SId (Identifier{id = "foo"}), SSExp [SInteger 1, SInteger 2, SId (Identifier{id = "spam"})]]
    describe "sexpParser" do
        it "parses a double" do
            parse sexpParser "" "(42.1f)" `shouldParse` SSExp [SDouble 42.1]
        it "parses a double without a dot" do
            parse sexpParser "" "(42f)" `shouldParse` SSExp [SDouble 42.0]
        it "parses an s expression preceded by white space" do
            parse sexpParser "" "   (foo bar baz 23)   " `shouldParse` SSExp [SId (Identifier{id = "foo"}), SId (Identifier{id = "bar"}), SId (Identifier{id = "baz"}), SInteger 23]
        it "doesn't parse when there is a non space remainder" do
            parse sexpParser "" `shouldFailOn` "    (1 2 3)    remainder"
