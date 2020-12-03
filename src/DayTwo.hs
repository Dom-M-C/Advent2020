{-# LANGUAGE OverloadedStrings #-}
module DayTwo (validPolicyCount, validTobogganCorporatePolicyCount) where

import qualified Data.Text as T

testInput :: [T.Text]
testInput = 
    [   "1-3 a: abcde"
    ,   "1-3 b: cdefg"
    ,   "2-9 c: ccccccccc"
    ]

data PasswordLine = PasswordLine 
    {   passwordPolicyToken :: T.Text
    ,   passwordToken :: T.Text
    } deriving (Show)

createPasswordLine :: T.Text -> PasswordLine
createPasswordLine str = PasswordLine (policy str) (passwd str)
    where
        baseToken   = T.breakOn ":"
        policy      = fst . baseToken
        passwd      = T.drop 1 . snd . T.breakOn " " . snd . baseToken

data PasswordPolicy = PasswordPolicy 
    {   minOccurences   :: Integer
    ,   maxOccurences   :: Integer
    ,   luckyLetter     :: T.Text
    ,   password        :: T.Text
    } deriving(Show)

mapToPasswordPolicy :: PasswordLine -> PasswordPolicy
mapToPasswordPolicy (PasswordLine token pwd) = PasswordPolicy minO maxO ll pwd
    where
        brokenToken = T.breakOn "-" token
        minO = (read . T.unpack . fst $ brokenToken) :: Integer
        maxAndLetter = T.breakOn " " . T.drop 1 . snd $ brokenToken
        maxO = (read . T.unpack . fst $ maxAndLetter) :: Integer
        ll = T.drop 1 $ snd maxAndLetter


isPasswordValid :: PasswordPolicy -> Bool
isPasswordValid (PasswordPolicy minO maxO ll pwd) = luckyCharCount >= minO && luckyCharCount <= maxO
    where
        luckyPasswordChars = [c | c <- T.unpack pwd, c == head (T.unpack ll)]
        luckyCharCount = toInteger $ length luckyPasswordChars

processPolicies :: [T.Text] -> [PasswordPolicy]
processPolicies = map $ mapToPasswordPolicy . createPasswordLine

validPolicies :: [T.Text] -> [PasswordPolicy]
validPolicies txtLines = [policy | policy <- processPolicies txtLines, isPasswordValid policy]

validPolicyCount :: [T.Text] -> Int
validPolicyCount = length . validPolicies

isPasswordValidTobogganCorporatePolicy :: PasswordPolicy -> Bool
isPasswordValidTobogganCorporatePolicy (PasswordPolicy minO maxO ll pwd) = xor (firstIndex == ll) (secondIndex == ll)
    where
        getTextAtTobogganIndex i = T.pack [T.index pwd $ fromIntegral i - 1]
        firstIndex  = getTextAtTobogganIndex minO
        secondIndex = getTextAtTobogganIndex maxO
        xor a b = (a || b) && not (a && b)

validTobogganCorporatePolicies :: [T.Text] -> [PasswordPolicy]
validTobogganCorporatePolicies txtLines = [policy | policy <- processPolicies txtLines, isPasswordValidTobogganCorporatePolicy policy]

validTobogganCorporatePolicyCount :: [T.Text] -> Int
validTobogganCorporatePolicyCount = length . validTobogganCorporatePolicies
