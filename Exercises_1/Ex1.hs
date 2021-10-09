-- | Exercise 1 template.
-- Instructions. 
--      1. Write your solutions to the exercise in this file. You
--      may add extra functions in this file to implement your solution.
--
--      2. Do NOT change the type or name of any of the template functions. 
--
--      3. Do NOT change the name of this file. 
--
--      4. Do NOT change the module name of this file.
--
--      5. To submit this to GradeScope, submit ONLY this file.
--
--      6. Have lots of fun :)
module Ex1 where

-- This includes the required types for this assignment (namely, the 'SF' type).
import Ex1Types

-- This imports some standard library functions for you to use. 
import Prelude (Int, Float, Integer, Eq, Ord, Bool (..), String, otherwise, abs, (+), (-), subtract, (*), (/), (==), (/=), (<), (<=), (>), (>=), (||), (&&), rem, mod, div, quot, max, min, fromIntegral, undefined, error, show)
-- This includes helpful functions for debugging.
import Debug.Trace

-- A supplementary function that combines together two lists 
appendLists :: [a] -> [a] -> [a]
appendLists xs []           = xs
appendLists [] ys           = ys
appendLists (x:xs) (ys)     = x:appendLists xs ys   

-- A supplementary function that folds
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f z = go
  where
    go [] = z
    go (a:as) = a `f` go as

-- A supplmentary function that maps
myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldr go []
  where
    go a as = f a : as


-- The fromIntegral converts the integer inputs into floating point values which now produce a floating point value upon division.
-- | Q1.
avgThree :: Int -> Int -> Int -> Float
avgThree x y z = ((fromIntegral x) + (fromIntegral y) + (fromIntegral z))/3

 
-- The function first computes which is the biggest of a,b,c and this creates the first part of the tuple. Then the number of  times  it appears is then calculated. The auxiliary functions maxiOfabc is what calculates the largest, and boolyToInt converts a bool to 1 or 0. 
-- This is code taken from tutorial session and is TA's code.
-- | Q2.
maxThree :: Int -> Int -> Int -> (Int, Int)
maxThree a b c = 
    ( maxiOfabc
    , boolyToInt (maxiOfabc == a) + boolyToInt (maxiOfabc == b) + boolyToInt (maxiOfabc == c) 
    )
  where
    myMaxi :: Int -> Int -> Int
    myMaxi a b 
        | a > b = a
        | otherwise = b

    maxiOfabc :: Int
    maxiOfabc = myMaxi (myMaxi a b) c

    boolyToInt :: Bool -> Int
    boolyToInt True = 1
    boolyToInt False = 0

 
-- If as input 0 or 1 is entered, we can immediately return 1,  anything less and the factorial doesn't exist so we can return FF. Otherwise we go to largestFac which starting from  1, checks if factorial of the integer above it is a possible answer, if yes, we increment to that integer. Otherwise we return the current integer
-- | Q3.
invFac :: Integer -> SF Integer
invFac a 
    | a == 0 || a == 1  = SS 1
    | a < 0             = FF        
    | otherwise         = largestFac a 1   
    where              
        largestFac :: Integer -> Integer -> SF Integer
        largestFac x y 
            | factOf y+1 <= x   = largestFac x (y+1) 
            | otherwise         = SS (y-1)  
            where
                factOf :: Integer -> Integer 
                factOf 0 = 1
                factOf a = a * factOf (a-1) 

-- Function takes the larger of the two given values and checks if it is divisor of both ints. If it is return it, else decrement it by 1 and check again if it is divisor until we reach base case of 1. We also use absolutes because the sign doesn't affect GCD. If either ints given as input are 0, we can quickly return the other to avoid division by zero error.
-- | Q4. 
myGcd :: Int -> Int -> Int
myGcd x y 
    | x == 0            = y
    | y == 0            = x  
    | abs x < abs y     = calculateDiv (abs x) (abs x) (abs y) 
    | otherwise         = calculateDiv (abs y) (abs y) (abs x)
    where   
        calculateDiv :: Int -> Int -> Int -> Int 
        calculateDiv a b c  
            | a == 1                            = 1
            | b `rem` a == 0 && c `rem` a == 0  = a
            | otherwise                         = calculateDiv (a-1) b c

 
-- This is code taken from tutorial session and is TA's code.
-- | Q5.
binom :: Integer -> Integer -> Integer
binom n k = multiplyInRange (n - k + 1) n `div` factorial k
    where
        factorial :: Integer -> Integer
        factorial n
            | n <= 0 = 1
            | otherwise = n * factorial (n - 1)

        -- Given two integers a,b, we want to multiply all numbers between a and b
        -- inclusive; and return 1 if a > b.
        multiplyInRange :: Integer -> Integer -> Integer
        multiplyInRange a b 
            | a > b = 1
            | otherwise = a * multiplyInRange (a + 1) b


-- Function grows String by taking it character by character and growing it based on a counter that starts from 1 for the first element and 2 for the second element etc. Makes use of appendLists which merges two lists, and replicateThis which given i j n, will replicate n j-i times 
-- | Q6. 
grow :: String -> String
grow [] = []
grow a  = makeGrow 1 a []
    where
        makeGrow :: Int -> String -> String -> String 
        makeGrow x (z:zs) []
            | zs == []          = (z:zs)  
            | otherwise         = makeGrow 2 zs (z:[])  
        makeGrow x (z:zs) (w:ws)
            | zs == []          = appendLists (w:ws) (replicateThis 1 x [z])
            | otherwise         = makeGrow (x+1) zs (appendLists (w:ws) (replicateThis 1 x [z])) 
            where
                replicateThis :: Int -> Int -> String -> String
                replicateThis a b (c:cs)  
                    | a == b    = (c:cs)
                    | otherwise = replicateThis (a+1) b (appendLists [c] (c:cs)) 


 
-- Function creates 2 element tuples of the input list (for list [w,x,y,z] (w,x) (x,y) (y,z) ) and if any don't follow the strictly increasing rule i.e. if first element is not smaller than second element, return false, else return true
-- | Q7.
instrictorder :: [Int] -> Bool
instrictorder [] = True
instrictorder (a:as) = all go (zipL (a:as) as)
  where
    go (a,b) = a < b

    zipL :: [a] -> [b] -> [(a,b)]
    zipL [] [] = []
    zipL (a : as) (b : bs) = (a,b) : zipL as bs
    zipL [] _ = []
    zipL _ [] = []

    -- | Is the predicate true for EVERY element in the list?
    all :: (a -> Bool) -> [a] -> Bool
    all f lst = go lst
        where
            -- go :: [b] -> Bool
            go [] = True
            go (a:as) = f a && go as


-- Uses list comprehension to determine if for the given tuple list, the integer or "cost" is smaller than our threshold. If it isn't, the string makes the list, if it is we disregard it.
-- | Q8. 
cheapItems :: [(String, Int)] -> Int -> [String]
cheapItems lst a = [ (xstr) | (xstr,xint) <- lst, isSmaller xint a]
    where 
        isSmaller :: Int -> Int -> Bool
        isSmaller x y = x < y 


-- This is code taken from tutorial session and is TA's code.
-- | Q9. 
sortByCost :: [(String, Int)] -> [(String, Int)]
sortByCost [] = []
sortByCost lst = go (myMap (\a -> [a]) lst)
  where 
    go [lst] = lst
    go lsts = go (merger lsts)

    merger (s0:s1:ss) = merge s0 s1 : merger ss
    merger ss = ss

    merge [] [] = []
    merge (a:as) [] = a:as
    merge [] (b:bs) = b:bs
    merge ((astr, aint):as) ((bstr,bint):bs) 
        | aint <= bint = (astr, aint) : merge as ((bstr,bint):bs)
        | otherwise = (bstr, bint) : merge ((astr, aint) : as) bs


-- If the integer provided is 0 or 1, the empty list is returned, otherwise we make up two lists. The first is 2-7 and if they're divisors are added to list. The second are 9-a/2. The two lists are separated because we test for primality using 2-7 so despite them being prime numbers, they'd all fail the isPrime test.
-- | Q10. 
divisors :: Integer -> [Integer]
divisors 0 = []
divisors 1 = []
divisors a = appendLists [ n | n<-[2,3,5,7], a `rem` n == 0] [ n | n<- (appendLists [9,11..a `div` 2] [a]), isPrime n, a `rem` n == 0]
    where  
        isPrime :: Integer -> Bool 
        isPrime a 
            | a `rem` 2 == 0    = False 
            | a `rem` 3 == 0    = False 
            | a `rem` 5 == 0    = False 
            | a `rem` 7 == 0    = False 
            | otherwise         = True 


-- This is code taken from tutorial session and is TA's code.
-- | Q11. 
myIsPrefixOf :: String -> String -> Bool
myIsPrefixOf [] [] = True
myIsPrefixOf (a:as) [] = False
myIsPrefixOf [] (b:bs) = True
myIsPrefixOf (a:as) (b:bs) 
    | a == b = myIsPrefixOf as bs
    | otherwise = False

substring :: String -> String -> Bool
substring [] _ = True
substring needle haystack = go haystack
  where
    go [] = False
    go (a:as) = myIsPrefixOf needle (a:as) || go as 


-- | Q12. 
sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists lst = allPerms lst [[]]
    where 
        allPerms :: [a] -> [[a]] ->[[a]]
        allPerms (x:xs) (y:ys)
            | 
