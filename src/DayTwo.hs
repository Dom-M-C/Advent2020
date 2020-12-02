{-# LANGUAGE OverloadedStrings #-}
module DayTwo where

import qualified Data.Text as T

testInput :: [T.Text]
testInput = 
    [   "1-3 a: abcde"
    ,   "1-3 b: cdefg"
    ,   "2-9 c: ccccccccc"
    ]

data PasswordLine = PasswordLine 
    {   passwordPolicyToken :: String
    ,   password :: String
    }

class ParsePasswordLine a where
    getCreatePasswordLine :: String -> a


instance ParsePasswordLine PasswordLine where
    getCreatePasswordLine str = undefined
