module Main where

import Test.Hspec
-- import Test.QuickCheck
-- import Control.Exception (evaluate)

main :: IO ()
main = hspec $
    description "Specs" $
        it "should have something." $
            pending "Fill in."

