module DayThree where

data AreaSpace = Open | Tree deriving(Show)

type TreeLine = [AreaSpace]

testInput = 
    [   "..##......."
    ,   "#...#...#.."
    ,   ".#....#..#."
    ,   "..#.#...#.#"
    ,   ".#...##..#."
    ,   "..#.##....."
    ,   ".#.#.#....#"
    ,   ".#........#"
    ,   "#.##...#..."
    ,   "#...##....#"
    ,   ".#..#...#.#"
    ]

transformStringToAreaSpace :: Char -> AreaSpace
transformStringToAreaSpace '.' = Open
transformStringToAreaSpace '#' = Tree

mapArea :: [[Char]] -> [TreeLine]
mapArea = map . map $ transformStringToAreaSpace

makeInfinite = map cycle . mapArea

data Toboggan = Toboggan
    {   position :: (Integer, Integer)
    ,   treesHit :: Integer
    ,   lateralVelocity :: Int
    ,   longitudinalVelocity :: Int
    }

constructToboggan :: Int -> Int -> Toboggan
constructToboggan latV longV = Toboggan 
    {   position = (0, 0)
    ,   treesHit = 0
    ,   lateralVelocity = latV
    ,   longitudinalVelocity = longV
    }


