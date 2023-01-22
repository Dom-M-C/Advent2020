module DayOne 
    (   getValidExpensesPairs
    ,   getValidExpensesTriples
    ,   getTestPairs
    ,   getTestTriples
    ) where


import Data.Set as Set (fromList, toList)

type Expense = Integer

data Expenses = Expenses {
    expenses       :: [Expense],
    sumExpense     :: Expense,
    productExpense :: Expense
} deriving(Show)

instance Eq Expenses where
    (==) (Expenses _ exOne _) (Expenses _ exTwo _) = exOne == exTwo

instance Ord Expenses where
    (<=) (Expenses _ exOne _) (Expenses _ exTwo _) = exOne <= exTwo

createExpensesPairs :: [Expense] -> [Expenses]
createExpensesPairs xs = [Expenses [a, b] (a + b) (a * b)
    | a <- xs
    , b <- xs
    , isSum2020 [a, b]]

createExpensesTriples :: [Expense] -> [Expenses]
createExpensesTriples xs = [Expenses triple (sum triple) (product triple)
    | a <- xs
    , b <- xs
    , c <- xs
    , let triple = [a, b, c]
    , isSum2020 triple]

isSum2020 :: (Eq a, Num a) => [a] -> Bool
isSum2020 xs = sum xs == 2020

dedupeAndGetExpense :: [Expenses] -> [Expense]
dedupeAndGetExpense = map productExpense . toList . fromList

getValidExpensesPairs :: [Expense] -> [Expense]
getValidExpensesPairs = dedupeAndGetExpense . createExpensesPairs

getValidExpensesTriples :: [Expense] -> [Expense]
getValidExpensesTriples = dedupeAndGetExpense . createExpensesTriples

testExample :: [Integer]
testExample = 
    [  1721
    ,  979
    ,  366
    ,  299
    ,  675
    ,  1456
    ]

getTestPairs = getValidExpensesPairs testExample

getTestTriples = getValidExpensesTriples testExample
