module Main where

import Test.Hspec
-- import Test.QuickCheck
-- import Control.Exception (evaluate)

main :: IO ()
main = hspec $
    describe "Specs" $
        it "should have something." $
            pending "Fill in."

