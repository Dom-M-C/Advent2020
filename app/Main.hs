module Main where

import DayOne(getValidExpensesPairs, getValidExpensesTriples)
import DayTwo(validPolicyCount, validTobogganCorporatePolicyCount)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = dayOnePartOne

dayOneData :: IO [Integer]
dayOneData = do 
    fileContent <- readFile "./data/expenseReport"
    let fileLines = lines fileContent
    return $ map (\x -> read x :: Integer) fileLines

dayOnePartOne :: IO ()
dayOnePartOne = do
    d <- dayOneData
    print $ head $ getValidExpensesPairs d

dayOnePartTwo :: IO ()
dayOnePartTwo = do
    d <- dayOneData
    print $ head $ getValidExpensesTriples d

--dayTwoData :: IO Int
dayTwoData = do
    d <- TIO.readFile "./data/passwordPolicies"
    return $ T.lines d

dayTwoPartOne :: IO Int
dayTwoPartOne = do
    d <- dayTwoData
    let count = validPolicyCount d
    return count

dayTwoPartTwo :: IO Int
dayTwoPartTwo = do
    d <- dayTwoData
    let count = validTobogganCorporatePolicyCount d
    return count
