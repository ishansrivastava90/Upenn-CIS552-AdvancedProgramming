Advanced Programming - CIS 552 - Lec 1
=======================================

Lists
------

l1 :: [Int]
l1 = [1, 2, 3, 4]

l2 :: [[Int]]
l2 = [[1, 2, 3], [5, 10], []]

cons :: a -> [a] -> [a]
cons = (:)


Step1 - Write tests
testClone1, testClone2, testClone3 :: Test
testClone1 = clone 'a' 4 ~?= ['a', 'a', 'a', 'a']
testClone2 = clone 'a' 0 ~?= []
testClone3 = clone 1.1 2 ~?= [1.1, 1.1]

Step2 - Define type of function 
clone :: a -> Int -> [a]

Step3 - Implement the method
clone x n = if n==0 then [] else x: clone x (n-1) 
OR (Better Way) 
clone' :: a -> Int -> [a]
clone' x 0 = []
clone' x n = x : clone' x (n-1)

OR (Even Better)
clone' x n | n==0 = []
		   | otherwise clone' x (n-1)

Step4 - Run the tests

===============================================================================




