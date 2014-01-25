module Main (main, spec)
where

import Test.Hspec
import Control.Arrow (left)

import Parser

main :: IO ()
main = hspec spec

parseTest :: Parser a -> String -> Either String a
parseTest p input = (left show $ parse p "Spec" input)

spec :: Spec
spec = do
        describe "funcCall" $ do
            context "with valid one" $ do
                it "parses" $ do
                    parseTest funcCall "f a b" `shouldBe`
                        (Right $ FuncCall "f" [Identifier "a", Identifier "b"])
