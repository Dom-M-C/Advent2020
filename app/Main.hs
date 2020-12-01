module Main where

import DayOne(getValidExpensesPairs, getValidExpensesTriples)

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
