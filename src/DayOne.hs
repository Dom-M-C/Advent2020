module DayOne where

import Control.Monad(guard)
import Data.Set as Set (fromList, toList)

type Expense = Integer

expenseReport :: [Expense]
expenseReport = [1000..2020]

data ExpensePair = Pair {
    first   :: Expense,
    second  :: Expense,
    sum     :: Expense,
    product :: Expense
} deriving(Show)

instance Eq ExpensePair where
    (==) (Pair _ _ fstSum _) (Pair _ _ sndSum _) = fstSum == sndSum

instance Ord ExpensePair where
    (<=) (Pair _ _ fstSum _) (Pair _ _ sndSum _) = fstSum <= sndSum

createExpensePair :: (Expense, Expense) -> ExpensePair
createExpensePair (fstEx, sndEx) = Pair fstEx sndEx (fstEx + sndEx) (fstEx * sndEx)

isSum2020 :: (Eq a, Num a) => a -> a -> Bool
isSum2020 x y = x+y == 2020

valid2020Pairs :: [(Expense, Expense)] -> [(Expense, Expense)]
valid2020Pairs p = do
    vp@(x, y) <- p
    guard(isSum2020 x y)
    return vp

filterValidExpensePairs :: [(Expense, Expense)] -> [ExpensePair]
filterValidExpensePairs = Set.toList . Set.fromList . map createExpensePair . valid2020Pairs

crossJoinExpenses :: [Expense] -> [(Expense, Expense)]
crossJoinExpenses xs = --toList . fromList $ 
    (,) <$> xs <*> xs

getValidExpenses :: [Expense] -> [ExpensePair]
getValidExpenses = filterValidExpensePairs . crossJoinExpenses

testExample :: [Integer]
testExample = 
    [  1721
    ,  979
    ,  366
    ,  299
    ,  675
    ,  1456
    ]

getTestPair = getValidExpenses testExample
--getPairs2020 testExample