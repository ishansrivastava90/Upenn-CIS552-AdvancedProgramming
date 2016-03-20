{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

{-# LANGUAGE NoImplicitPrelude #-}

{-
  Assignment: 1
  Author: ishan
  Date: Sep 1, 2015
-}

module Main where
import Prelude  hiding (all, reverse, takeWhile, zip, concat, concatMap)
import Test.HUnit 

main :: IO ()
main = do 
   _ <- runTestTT $ TestList [ testStyle,
                               testLists,
                               testBowlingKata,
                               testLcs ]
   return ()

--------------------------------------------------------------------------------

testStyle :: Test
testStyle = "testStyle" ~:
   TestList [ tabc , tarithmetic, treverse, tapplyFunc ]

-- | Evaluating a boolean expression.
abc :: Bool -> Bool -> Bool -> Bool
abc x y z = x && (y || z)

tabc :: Test
tabc = "abc" ~: TestList [abc True False True ~?= True, 
                          abc True False False ~?= False,
                          abc False True True ~?= False]

-- | Computes value of arithmetic expression.
arithmetic :: ((Int, Int), Int) -> ((Int, Int), Int) -> (Int, Int, Int)
arithmetic ((a, b), c) ((d, e), f) =  (b*f - c*e, c*d - a*f, a*e - b*d)


tarithmetic :: Test
tarithmetic = "arithmetic" ~:
   TestList[ arithmetic ((1,2),3) ((4,5),6) ~?= (-3,6,-3), 
             arithmetic ((3,2),1) ((4,5),6) ~?= (7,-14,7) ]

-- | Reverses a list.
reverse :: [a] -> [a]
reverse []       = []
reverse (x:xs) = reverse xs ++ [x]

-- OR
-- reverse :: [a] -> [a]
-- reverse = foldl (flip (:)) []

treverse :: Test
treverse = "reverse" ~: TestList [reverse [3,2,1] ~?= [1,2,3],
                                  reverse [1]     ~?= [1] ]

-- | Apply functions to corresponding arguments.
applyFunc :: [a -> b] -> [a] -> [b]
applyFunc (f : fs) (x : xs) =  f x : applyFunc fs xs 
applyFunc _ _               = []

tapplyFunc:: Test
tapplyFunc = "applyFunc" ~:
  TestList [ applyFunc [ (+1), \n -> n - 1, (+1) ]
                   ([3, 4, 5] :: [Int]) ~?= [4,3,6],
             applyFunc [ null, not . null ] [ [], "a" ] ~?= [True, True],
             applyFunc [] "a" ~?=  "",
             applyFunc [not] [] ~?= []]

-------------------------------------------------------------------------------- 

testLists :: Test
testLists = "testLists" ~: TestList [tintersperse, tinvert, ttakeWhile, tfind, tall, tmap2, tzip, ttranspose, tconcat]

-- The intersperse function takes an element and a list 
-- and intersperses that element between the elements of the list. 
-- For example,
--    intersperse ',' "abcde" == "a,b,c,d,e"

intersperse :: a -> [a] -> [a] 
intersperse _ []           = []
intersperse _ [x]          = [x]
intersperse element (x:xs) = x:element:intersperse element xs

tintersperse :: Test
tintersperse = "intersperse" ~:
  TestList [ intersperse '_' [] ~?= [],
             intersperse '-' "i" ~?= "i",
             intersperse ',' "abcde" ~?= "a,b,c,d,e",
             intersperse 0 [1,2,3] ~?= [1,0,2,0,3]]


-- invert lst returns a list with each pair reversed. 
-- for example:
--   invert [("a",1),("a",2)]     returns [(1,"a"),(2,"a")] 
--   invert ([] :: [(Int,Char)])  returns []

--   note, you need to add a type annotation to test invert with []

invert :: [(a,b)] -> [(b,a)]
invert []          = []
invert ((a,b):xs) = (b,a) : invert xs 

tinvert :: Test
tinvert = "invert" ~: 
  TestList [ invert ([] :: [(Int, Char)]) ~?= [],
             invert [(1, "a"), (4, "z")] ~?= [("a", 1), ("z", 4)] ]


-- takeWhile, applied to a predicate p and a list xs, 
-- returns the longest prefix (possibly empty) of xs of elements 
-- that satisfy p:
-- For example, 
--     takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
--     takeWhile (< 9) [1,2,3] == [1,2,3]
--     takeWhile (< 0) [1,2,3] == []

takeWhile :: (a->Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs) 
  | p x    = x : takeWhile p xs
  | otherwise = []

ttakeWhile :: Test
ttakeWhile = "takeWhile" ~:
  TestList [ takeWhile (> 1) [] ~?= [],
             takeWhile (< 3) [1,2,3,4,5] ~?= [1,2],
             takeWhile (== 'a') "aabcad" ~?= "aa",
             takeWhile (> 10) [1,3,4,7,10] ~?= [] ]
 

-- find pred lst returns the first element of the list that 
-- satisfies the predicate. Because no element may do so, the 
-- answer is returned in a "Maybe".
-- for example: 
--     find odd [0,2,3,4] returns Just 3

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x:xs) 
  | p x = Just x
  | otherwise = find p xs


tfind :: Test
tfind = "find" ~:
  TestList [ find (> 100) ([] :: [Int]) ~?= Nothing,
             find even [1,3,2,4,6] ~?= Just 2,
             find (== 0) [1,3,5] ~?= Nothing ]

 

-- all pred lst returns False if any element of lst fails to satisfy
-- pred and True otherwise.
-- for example:
--    all odd [1,2,3] returns False

all :: (a -> Bool) -> [a] -> Bool
all p = foldr (\x y -> p x && y ) True

tall :: Test
tall = "all" ~:
  TestList [ all (== 0) ([] :: [Int]) ~?= True,
             all odd [1,3,5] ~?= True,
             all even [2,3,4] ~?= False ]



-- map2 f xs ys returns the list obtained by applying f to 
-- to each pair of corresponding elements of xs and ys. If 
-- one list is longer than the other, then the extra elements 
-- are ignored.
-- i.e. 
--   map2 f [x1, x2, ..., xn] [y1, y2, ..., yn, yn+1] 
--        returns [f x1 y1, f x2 y2, ..., f xn yn]
--
-- NOTE: map2 is called zipWith in the standard library.

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 _ [] _          = []
map2 _ _ []          = []
map2 f (x:xs) (y:ys) = f x y : map2 f xs ys 

tmap2 :: Test
tmap2 = "map2" ~:
  TestList [ map2 (+) ([] ::[Int]) ([] ::[Int]) ~?= [],
             map2 (+) [1,2,3] [10,20,30,40] ~?= [11,22,33],
             map2 (\x y -> (x,y)) [1,2,3,4] "abc" ~?= [(1,'a'),(2,'b'),(3,'c')] ]



-- zip takes two lists and returns a list of corresponding pairs. If
-- one input list is shorter, excess elements of the longer list are
-- discarded.
-- for example:  
--    zip [1,2] [True] returns [(1,True)]

zip :: [a] -> [b] -> [(a,b)]
zip _ [] = []
zip [] _ = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

tzip :: Test
tzip = "zip" ~:
  TestList [ zip ([] :: [Int]) "abc" ~?= [],
             zip [1,2,3] ["one","two","three"]
               ~?= [(1,"one"),(2,"two"),(3,"three")],
             zip [1,2] [0.1,0.2,0.3] ~?= [(1,0.1),(2,0.2)] ]



-- transpose  (WARNING: this one is tricky!)
-- The transpose function transposes the rows and columns of its argument. 
-- If the inner lists are not all the same length, then the extra elements
-- are ignored.
-- for example:
--    transpose [[1,2,3],[4,5,6]] returns [[1,4],[2,5],[3,6]]

ttranspose :: Test
ttranspose = "transpose" ~: assertFailure "testcase for transpose"



-- concat
-- The concatenation of all of the elements of a list of lists
-- for example:
--    concat [[1,2,3],[4,5,6],[7,8,9]] returns [1,2,3,4,5,6,7,8,9]
 
concat ::[[a]] -> [a]
concat = foldr (++) []
 
tconcat :: Test
tconcat = "concat" ~: 
  TestList [ concat ([] ::[[Int]]) ~?= [],
             concat [[1,2],[],[5,6,7]] ~?= [1,2,5,6,7],
             concat [[1,2,3],[4,5],[6,7,9]] ~?= [1,2,3,4,5,6,7,9] ]



-- concatMap
-- Map a function over all the elements of the list and concatenate the results.
-- for example:
--    concatMap (\x -> [x,x+1,x+2]) [1,2,3]  returns [1,2,3,2,3,4,3,4,5]

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f l = concat [f x | x <- l]

tconcatMap :: Test
tconcatMap = "concatMap" ~: 
  TestList [ concatMap (\x -> [x,x+1,x+2]) [1,2,3] ~?= [1,2,3,2,3,4,3,4,5] ]
             

--------------------------------------------------------------------------------

bowlingTest0 :: ([Int] -> Int) -> Test
bowlingTest0 score = "all gutter balls" ~: 0 ~=? score (replicate 20 0)

score0 :: [ Int ] -> Int
score0 _ = 0

bowlingTest1 :: ([Int] -> Int) -> Test
bowlingTest1 score = 
   "allOnes" ~: 20 ~=? score (replicate 20 1)

score1 :: [ Int ] -> Int
score1 = score where
   score [] = 0
   score (x:xs) = x + score xs

bowlingTest2 :: ([ Int ] -> Int) -> Test
bowlingTest2 score = 
   "always fail" ~: 29 ~?= score ([3,7] ++ (replicate 18 1))

score2 :: [ Int ] -> Int
score2 = score where
  score []    = 0
  score [x]   = x
  score (x:y:xs) 
    | (x+y == 10) = 10 + bonus xs + score xs
    | otherwise = x + y + score xs

bonus :: [ Int ] -> Int
bonus [] = 0
bonus (x:_) = x

score2a :: [ Int ] -> Int
score2a = score where
   score = score2

bowlingTest3 :: ([ Int ] -> Int) -> Test
bowlingTest3 score = 
   "always fail" ~: 44 ~?= score ([10] ++[3,6] ++ (replicate 16 1))

score3 :: [ Int ] -> Int
score3 = score where
  score []    = 0
  score [x]   = x
  score (x:y:xs) 
    | (x == 10) = 10 + y + bonus xs + score (y:xs)
    | (x+y == 10) = 10 + bonus xs + score xs
    | otherwise = x + y + score xs

bowlingTest4 :: ([ Int ] -> Int) -> Test
bowlingTest4 score = "perfect game" ~: 300 ~=? score (replicate 12 10) 

score4 :: [ Int ] -> Int
score4 = score where
  score (x:y:xs) 
    | (x == 10 && length (y:xs) == 2)  = 10 + strikeBonus (y:xs)  -- Case: Last Strike with 2 additional bonus.
    | (x == 10 && length (y:xs) > 2)   = 10 + strikeBonus (y:xs) + score (y:xs)
    | (x+y == 10)                      = 10 + spareBonus xs + score xs
    | otherwise                        = x + y + score xs
    where
      spareBonus (a:_)    = a
      spareBonus _        = 0
      strikeBonus (a:b:_) = a + b
      strikeBonus _       = 0
  score _    = 0

score5 :: [ Int ] -> Int
score5 = score where
  score (x:y:xs) 
    | x == 10 && length xs == 1 = 10 + strikeBonus (y:xs) -- Case: Last Strike with 2 additional bonus.
    | x == 10 && length xs > 1  = 10 + strikeBonus (y:xs) + score (y:xs)
    | x + y == 10               = 10 + spareBonus xs + score xs
    | otherwise                 = x + y + score xs
    where
      spareBonus (a:_)    = a
      spareBonus _        = 0
      strikeBonus (a:b:_) = a + b
      strikeBonus _       = 0
  score _    = 0

bowlingTest5 :: ([ Int ] -> Int) -> Test
bowlingTest5 score = "Complex game" ~: 133 ~=? score [4,2,5,3,2,2,10,10,4,5,7,3,2,4,2,8,10,10,5]


testBowlingKata :: Test
testBowlingKata = TestList (map checkOutput scores) where
  -- the five test cases, in order 
  bowlingTests  = [bowlingTest0, bowlingTest1, bowlingTest2, 
                   bowlingTest3, bowlingTest4]
 
  -- the names of the score functions, the functions themselves, 
  -- and the expected number of passing tests
  scores = zip3 ['0' ..] [score0, score1, score2a, score3, score4] [1..]
 
  -- a way to run a unit test without printing output 
  testSilently = performTest (\ _ _ -> return ()) 
                   (\ _ _ _ -> return) (\ _ _ _ -> return) ()
 
  -- run each bowling test on the given score function, making sure that 
  -- the expected number of tests pass.
  checkOutput (name, score, pass) = " Testing score" ++ [name] ~: do 
    (s0,_) <- testSilently $ (TestList $ bowlingTests `applyFunc` (repeat score))
    assert $ pass @=? cases s0 - (errors s0 + failures s0)

-------------------------------------------------------------------------------- 

lcs :: String -> String -> String 
lcs = undefined -- s1@(x:xs) s2@(y:ys) 
--   | x == y = 
--   | otherwise = lcs s1 ys 

testLcs :: Test
testLcs = "Lcs" ~: 
   TestList [ lcs "Advanced" "Advantaged" ~?= "Advaned"]
--              lcs "abcd" "acbd" ~?= "acd",
--              lcs "aaaAbaa" "abaaaa" ~?= "aaaaa",
--              lcs "" "abc" ~?= "" ]




