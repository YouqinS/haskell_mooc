module Example where

import Data.List
import qualified Data.Map as Map --Data.Map contains some function with the same names as Prelude functions, the namespace needs to be imported qualified
import Data.Array


polynomial :: Double -> Double
polynomial x = x^2 - x - 1
f1 p = polynomial (p) - 1
f2 y = y + 1


-- if ... then ... else
login user password = if user == "unicorn73"
                      then if password == "f4bulous!"
                           then "unicorn73 logged in"
                           else "wrong password"
                      else "unknown user"

login1 :: String -> String -> String
login1 "unicorn73" "f4bulous!" = "unicorn73 logged in"
login1 "unicorn73" _           = "wrong password"
login1 _           _           = "unknown user"


-- creating local definitions: 
-- let...in and where
circleArea :: Double -> Double
circleArea r = pi * rsquare
    where pi = 3.1415926
          rsquare = r * r

{- circleArea r = let pi = 3.1415926
                   square x = x * x
               in pi * square r -}


x :: Int
x = 5

f :: Int -> Int
f x = 2 * x

g :: Int -> Int
g y = x where x = 6

h :: Int -> Int
h x = x where x = 3

-- Recursion
factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n-1)

{- factorial 3
  ==> 3 * factorial (3-1)
  ==> 3 * factorial 2
  ==> 3 * 2 * factorial 1
  ==> 3 * 2 * 1
  ==> 6 -}


-- factorial implemented with guards instead of pattern matching
factorial' n
  | n<0       = -1
  | n==0      = 1
  | otherwise = n * factorial' (n-1)


-- compute the sum 1^2+2^2+3^2+...+n^2
squareSum 0 = 0
squareSum n = n^2 + squareSum (n-1)


-- combine guards with pattern matching
guessAge :: String -> Int -> String
guessAge "Griselda" age
    | age < 47 = "Too low!"
    | age > 47 = "Too high!"
    | otherwise = "Correct!"
guessAge "Hansel" age
    | age < 12 = "Too low!"
    | age > 12 = "Too high!"
    | otherwise = "Correct!"
guessAge name age = "Wrong name!"



-- Fibonacci numbers, slow version
{- fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = fibonacci (n-2) + fibonacci (n-1) -}


-- fibonacci numbers, fast version
fibonacci :: Integer -> Integer
fibonacci n = fibonacci' 0 1 n

fibonacci' :: Integer -> Integer -> Integer -> Integer
fibonacci' a b 1 = b
fibonacci' a b n = fibonacci' b (a+b) (n-1)


repeatString n str = repeatHelper n str ""

-- repeatHelper n str result = if (n==0)
--                             then result
--                             else repeatHelper (n-1) str (result++str)

repeatHelper 0 _   result = result
repeatHelper n str result = repeatHelper (n-1) str (result++str)


-- Lists
{- 
  [True,True,False] :: [Bool]
  ["Moi","Hei"] :: [String]
  [] :: [a]                   -- more about this later
  [[1,2],[3,4]] :: [[Int]]    -- a list of lists
  [1..7] :: [Int]             -- range syntax, value [1,2,3,4,5,6,7] 
-}

{-
  head :: [a] -> a            -- returns the first element
  tail :: [a] -> [a]          -- returns everything except the first element
  init :: [a] -> [a]          -- returns everything except the last element
  take :: Int -> [a] -> [a]   -- returns the n first elements
  drop :: Int -> [a] -> [a]   -- returns everything except the n first elements
  (++) :: [a] -> [a] -> [a]   -- lists are catenated with the ++ operator
  (!!) :: [a] -> Int -> a     -- lists are indexed with the !! operator (get element at index of)
  reverse :: [a] -> [a]       -- reverse a list
  null :: [a] -> Bool         -- is this list empty?
  length :: [a] -> Int        -- the length of a list
-}


-- ghci> [7,10,4,5] !! 2
-- 4

-- ghci> f xs = take 2 xs ++ drop 4 xs  (take the first 2 and everything after the 4th)
-- ghci> f [1,2,3,4,5,6]
-- [1,2,5,6]


-- ghci> g xs = tail xs ++ [head xs]
-- ghci> g [1,2,3] 
-- [2,3,1]

-- ghci> reverse [1..4]
-- [4,3,2,1]

-- given a password, return (Just username) if login succeeds, Nothing otherwise
login2 :: String -> Maybe String
login2 "f4bulous!" = Just "unicorn73"
login2 "swordfish" = Just "megahacker"
login2 _           = Nothing

-- Multiply an Int with a Maybe Int. Nothing is treated as no multiplication at all.
perhapsMultiply :: Int -> Maybe Int -> Int
perhapsMultiply i Nothing = i
perhapsMultiply i (Just j) = i*j   -- Note how j denotes the value inside the Just

{- 
ghci> perhapsMultiply 1 Nothing 
1

ghci> perhapsMultiply 3 (Just 2)
6 

-}


intOrZero :: Maybe Int -> Int
intOrZero Nothing = 0
intOrZero (Just i) = i


-- ghci> intOrZero Nothing 
-- 0
-- ghci> intOrZero (Just 0)
-- 0
-- ghci> intOrZero (Just 1)
-- 1


safeHead :: [a] -> Maybe a
safeHead xs = if null xs then Nothing else Just (head xs)
-- ghci> safeHead []
-- Nothing
-- ghci> safeHead [1]
-- Just 1


headOrZero :: [Int] -> Int
headOrZero xs = intOrZero (safeHead xs)


readInt :: String -> Either String Int
readInt "0" = Right 0
readInt "1" = Right 1
readInt s = Left ("Unsupported string: " ++ s)

-- ghci> readInt "10"
-- Left "Unsupported string: 10"
-- ghci> readInt "1"
-- Right 1
-- ghci> readInt "0"
-- Right 0



iWantAString :: Either Int String -> String
iWantAString (Right str)   = str
iWantAString (Left number) = show number

-- ghci> iWantAString (Right "hi")
-- "hi"
-- ghci> iWantAString (Left 1)
-- "1"

lectureParticipants :: [Either String Int]
lectureParticipants = [Right 10, Right 13, Left "easter vacation", Right 17, Left "lecturer was sick", Right 3]

-- ghci> lectureParticipants
-- [Right 10,Right 13,Left "easter vacation",Right 17,Left "lecturer was sick",Right 3]


describe :: Integer -> String
describe n = case n of 0 -> "zero"
                       1 -> "one"
                       2 -> "an even prime"
                       n -> "the number " ++ show n



-- parse country code into country name, returns Nothing if code not recognized
parseCountry :: String -> Maybe String
parseCountry "FI" = Just "Finland"
parseCountry "SE" = Just "Sweden"
parseCountry _ = Nothing

flyTo :: String -> String
flyTo countryCode = case parseCountry countryCode of Just country -> "You're flying to " ++ country
                                                     Nothing -> "You're not flying anywhere"
-- ghci> flyTo "FI"
-- "You're flying to Finland"
-- ghci> flyTo "aa"
-- "You're not flying anywhere"


-- helper function for pattern matching instead of using the case-of expression:
flyTo1 :: String -> String
flyTo1 countryCode = handleResult (parseCountry countryCode)
  where handleResult (Just country) = "You're flying to " ++ country
        handleResult Nothing        = "You're not flying anywhere"


-- given a sentence, decide whether it is a statement, question or exclamation
sentenceType :: String -> String
sentenceType str = case last str of '.' -> "statement"
                                    '?' -> "question"
                                    '!' -> "exclamation"
                                    _   -> "not a sentence"

-- same function, helper function instead of case-of
sentenceType1 str = checkLastElement (last str)
  where checkLastElement "." = "statement"
        checkLastElement "?" = "question"
        checkLastElement "!" = "exclamation"
        checkLastElement _ = "not a sentence"


motivate :: String -> String
motivate "Monday"    = "Have a nice week at work!"
motivate "Tuesday"   = "You're one day closer to weekend!"
motivate "Wednesday" = "3 more day(s) until the weekend!"
motivate "Thursday"  = "2 more day(s) until the weekend!"
motivate "Friday"    = "1 more day(s) until the weekend!"
motivate _           = "Relax! You don't need to work today!"


motivate1 :: String -> String
motivate1 day = case distanceToSunday day of
  6 -> "Have a nice week at work!"
  5 -> "You're one day closer to weekend!"
  n -> if n > 1
       then show (n - 1) ++ " more day(s) until the weekend!"
       else "Relax! You don't need to work today!"


motivate2 :: String -> String
motivate2 day
  | n == 6 = "Have a nice week at work!"
  | n == 5 = "You're one day closer to weekend!"
  | n > 1 = show (n - 1) ++ " more day(s) until the weekend!"
  | otherwise = "Relax! You don't need to work today!"
  where n = distanceToSunday day - 1

distanceToSunday :: String -> Int
distanceToSunday d = case d of
  "Monday"    -> 6
  "Tuesday"   -> 5
  "Wednesday" -> 4
  "Thursday"  -> 3
  "Friday"    -> 2
  "Saturday"  -> 1
  "Sunday"    -> 0


area :: String -> Double -> Double
area shape x = case shape of
  "square" -> square x
  "circle" -> pi * square x
  where square x = x*x

-- getElement (Just i) gets the ith element (counting from zero) of a list, getElement Nothing gets the last element
getElement :: Maybe Int -> [a] -> a
getElement (Just i) xs = xs !! i
getElement Nothing xs = last xs

-- ghci> getElement Nothing [1,2]
-- 2
-- ghci> getElement (Just 1) [1,2]
-- 2
-- ghci> 

direction :: Either Int String -> String
direction (Left i) = "you should go left " ++ show i ++ " meters!"
direction (Right i) = "you should go right " ++ show i ++ " meters!"

-- ghci> direction (Left 1)
-- "you should go left 1 meters!"
-- ghci> direction (Right "1")
-- "you should go right \"1\" meters!"


addThree :: Int -> Int
addThree x = x + 3

doTwice :: (a -> a) -> a -> a
doTwice f x = f (f x)

-- ghci> doTwice addThree 1
-- 7
-- ghci> 


-- map :: (a -> b) -> [a] -> [b]
-- map addThree [1,2,3]
--   ==> [4,5,6]


-- filter :: (a -> Bool) -> [a] -> [a]
positive :: Int -> Bool
positive x = x>0

onlyPositive xs = filter positive xs

mapBooleans f = map f [False,True]


wrapJust xs = map Just xs
-- ghci> wrapJust [Nothing, Just 1]
-- [Just Nothing,Just (Just 1)]
-- ghci> wrapJust [2,1]
-- [Just 2,Just 1]


-- a predicate that checks if a string is a palindrome
palindrome :: String -> Bool
palindrome str = str == reverse str

-- ghci> palindrome "aba"
-- True



-- palindromes n takes all numbers from 1 to n, converts them to strings using show, and keeps only palindromes
palindromes :: Int -> [String]
palindromes n = filter palindrome (map show [1..n])
-- ghci> palindromes 100
-- ["1","2","3","4","5","6","7","8","9","11","22","33","44","55","66","77","88","99"]



getAWords :: String -> [[Char]]
getAWords string = filter startsWithA (words string)
  where startsWithA s = head s == 'a'

-- ghci> getAWords "does anyone want an apple?"
-- ["anyone","an","apple?"]


countAWords :: String -> Int
countAWords string = length (filter startsWithA (words string))
  where startsWithA s = head s == 'a'

-- ghci> countAWords "does anyone want an apple?"
-- 3


-- tails "echo"
--   ==> ["echo","cho","ho","o",""]


substringsOfLength :: Int -> String -> [String]
substringsOfLength n string = map shorten (tails string)
  where shorten s = take n s


-- ghci> tails "hello"
-- ["hello","ello","llo","lo","o",""]


-- ghci> substringsOfLength 3 "hello"
-- ["hel","ell","llo","lo","o",""]

whatFollows :: Char -> Int -> String -> [String]
whatFollows c k string = map tail (filter match (substringsOfLength (k+1) string))
  where match sub = take 1 sub == [c]

-- ghci> whatFollows 'a' 2 "abracadabra"
-- ["br","ca","da","br",""]


-- Partial Application

-- ghci> add x y = x + y
-- ghci> add 1 2
-- 3
-- ghci> addOne = add 1 (add remembers the first argument given)
-- ghci> addOne 2 (andd takes the second one when given)
-- 3

{- 
  ghci> :t add
  add :: Num a => a -> a -> a
  ghci> :t add 1
  add 1 :: Num a => a -> a
  ghci> :t add 1 2
  add 1 2 :: Num a => a 
-}


between :: Integer -> Integer -> Integer -> Bool
between lo high x = x < high && x > lo

-- ghci> map (between 1 3) [1,2,3]
-- [False,True,False]

{- 
  ghci> :t between 
  between :: Integer -> Integer -> Integer -> Bool
  ghci> :t between 1
  between 1 :: Integer -> Integer -> Bool
  ghci> :t between 1 3
  between 1 3 :: Integer -> Bool
  ghci> :t between 1 3 2
  between 1 3 2 :: Bool 
-}

-- ghci> map (drop 1) ["Hello", "World"]
-- ["ello","orld"]

-- ghci> map (*2) [1,2,3]
-- [2,4,6]
-- ghci> map (2*) [1,2,3]
-- [2,4,6]
-- ghci> map (1/) [1,2,4,5]
-- [1.0,0.5,0.25,0.2]


-- pass an operator as an argument to a function

-- ghci> :t zipWith
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- ghci> zipWith (+) [1,2] [3,4]
-- [4,6]

-- ghci> zipWith (++) ["John","Mary"] ["Smith","Cooper"]
-- ["JohnSmith","MaryCooper"]
-- ghci> zipWith take [4,3] ["Hello","Warden"]
-- ["Hell","War"]



-- ghci> 6 `div` 2
-- 3
-- ghci> div 6 2
-- 3
-- ghci> map (+1) [1,2,3]
-- [2,3,4]
-- ghci> (+1) `map` [1,2,3] 
-- [2,3,4]


-- Lambdas

-- ghci> filter (\x -> x>7) [1,10,100]
-- [10,100]

-- ghci> (\x -> x*x) 3
-- 9

-- ghci>  (\x -> reverse x == x) "ABBA"
-- True

-- ghci> filter (\x -> reverse x == x) ["ABBA","ACDC","otto","lothar","anna"]
-- ["ABBA","otto","anna"]

-- ghci> (\x y -> x^2+y^2) 2 3  -- multiple arguments
-- 13


--  . Operator: (.) :: (b -> c) -> (a -> b) -> a -> c


-- double x = 2*x
-- quadruple = double . double  -- computes 2*(2*x) == 4*x
-- f = quadruple . (+1)         -- computes 4*(x+1)
-- g = (+1) . quadruple         -- computes 4*x+1
-- third = head . tail . tail   -- fetches the third element of a list

-- ghci> filter (not . null) [[1,2,3],[],[4]]
-- [[1,2,3],[4]]


-- $ Operator: ($) :: (a -> b) -> a -> b
-- ghci> head (reverse "abcd")
-- 'd'
-- ghci> head $ reverse "abcd"
-- 'd'

-- putStrLn (show (1 + 1))
-- putStrLn (show $ 1 + 1)
-- putStrLn $ show (1 + 1)
-- putStrLn $ show $ 1 + 1


-- ($) is help avoid parentheses. 
-- (.) is for composing two functions together to make a new function



-- ghci> reverse (map head (map reverse (["Haskell","pro"] ++ ["dodo","lyric"])))
-- "cool"
-- ghci> (reverse . map head . map reverse) (["Haskell","pro"] ++ ["dodo","lyric"])
-- "cool"
-- ghci> reverse . map head . map reverse $ ["Haskell","pro"] ++ ["dodo","lyric"]
-- "cool"


-- map ($"string") [reverse, take 2, drop 2]
--   ==> [reverse $ "string", take 2 $ "string", drop 2 $ "string"]
--   ==> [reverse "string", take 2 "string", drop 2 "string"]
--   ==> ["gnirts", "st", "ring"]


{- 
  substringsOfLength :: Int -> String -> [String]
  substringsOfLength n string = map shorten (tails string)
    where shorten s = take n s

  whatFollows :: Char -> Int -> String -> [String]
  whatFollows c k string = map tail (filter match (substringsOfLength (k+1) string))
    where match sub = take 1 sub == [c] 
-}

-- whatFollows c k string = map tail (filter match (map shorten (tails string)))
--   where shorten s = take (k+1) s
--         match sub = take 1 sub == [c]

-- whatFollows c k string = map tail (filter match (map (take (k+1)) (tails string)))
--   where match sub = take 1 sub == [c]


-- whatFollows c k string = map tail . filter match . map (take (k+1)) $ tails string
--   where match sub = take 1 sub == [c]

-- whatFollows c k string = map tail . filter (\sub -> take 1 sub == [c]) . map (take (k+1)) $ tails string


-- whatFollows c k = map tail . filter ((==[c]) . take 1) . map (take (k+1)) . tails


-- takeWhile :: (a -> Bool) -> [a] -> [a]   take elements from beginning of a list until a predicate not satisfied
-- dropWhile :: (a -> Bool) -> [a] -> [a]   drop elements from beginning of a list as long as they satisfy a predicate

-- ghci> takeWhile even [2,4,1,2,3]
-- [2,4]
-- ghci> takeWhile even [1,2,1,3,4]
-- []
-- ghci> filter (\x -> even x) [2,4,1,2,3]
-- [2,4,2]

-- ghci> dropWhile even [2,1,3,4]
-- [1,3,4]
-- ghci> dropWhile even [1,2,1,3,4]
-- [1,2,1,3,4]


-- check if a list contains an element:
-- ghci> elem 3 [2,4,1,2,3]
-- True
-- ghci> elem 5 [2,4,1,2,3]
-- False


findSubstring :: String -> String -> String
findSubstring chars = takeWhile (\x -> elem x chars) -- "aa"
                      . dropWhile (\x -> not $ elem x chars) -- "aabaaaab"
-- ghci> findSubstring "a" "bbaabaaaab"
-- "aa"
-- ghci> findSubstring "abcd" "xxxyyyzabaaxxabcd"
-- "abaa"
-- ghci> 

-- The : operator builds a list out of a head and a tail. 
-- In other words, x : xs is the same as [x] ++ xs. 
-- ghci> :t (:)
-- (:) :: a -> [a] -> [a]

-- List patterns that end with :[] can be typed out as list literals. 
-- That is, just like [1,2,3] is the same value as 1:2:3:[], 
-- the pattern [x,y] is the same as the pattern x:y:[]. 
-- Let’s rewrite that previous example.

descend 0 = []
descend n = n : descend (n-1)
-- ghci> descend 4
-- [4,3,2,1]

-- ????????????
-- iterate f 0 x = [x]
-- iterate f n x = x : iterate f (n-1) (f x)

-- let xs = "terve"
-- in iterate tail (length xs) xs
--   ==> ["terve","erve","rve","ve","e",""]

-- split :: Char -> String -> [String]
-- split c [] = []
-- split c xs = start : split c (drop 1 rest)
--   where start = takeWhile (/=c) xs
--         rest = dropWhile (/=c) xs


sumFirstTwo :: [Integer] -> Integer
-- this equation gets used for lists of length at least two
sumFirstTwo (a:b:_) = a+b
-- this equation gets used for all other lists (i.e. lists of length 0 or 1)
sumFirstTwo _       = 0


-- ghci> sumFirstTwo []
-- 0
-- ghci> sumFirstTwo [1]
-- 0
-- ghci> sumFirstTwo [1,2]
-- 3


-- pattern matching
startsWithZero :: [Integer] -> Bool
startsWithZero (0:xs) = True
startsWithZero (x:xs) = False
startsWithZero []     = False


-- pattern matching and recursion
sumNumbers :: [Int] -> Int
sumNumbers [] = 0
sumNumbers (x:xs) = x + sumNumbers xs

-- Tail recursive version
-- it has the recursive call to go at the top level, i.e. in tail position.
{- 
sumNumbers :: [Int] -> Int
sumNumbers xs = go 0 xs
  where go sum [] = sum
        go sum (x:xs) = go (sum+x) xs
 -}


-- compute the largest number in a list
myMaximum :: [Int] -> Int
myMaximum [] = 0       -- actually this should be some sort of error...
myMaximum (x:xs) = go x xs
  where go biggest [] = biggest
        go biggest (x:xs) = go (max biggest x) xs

countNothings :: [Maybe a] -> Int
countNothings [] = 0
countNothings (Nothing : xs) = 1 + countNothings xs
countNothings (Just _  : xs) = countNothings xs
-- ghci> countNothings [Nothing, Just 1, Nothing]
-- 2

doubleList :: [Int] -> [Int]
doubleList [] = []
doubleList (x:xs) = 2*x : doubleList xs

-- doubleList [1,2,3]
-- === doubleList (1:(2:(3:[])))
-- ==> 2*1 : doubleList (2:(3:[]))
-- ==> 2*1 : (2*2 : doubleList (3:[]))
-- ==> 2*1 : (2*2 : (2*3 : doubleList []))
-- ==> 2*1 : (2*2 : (2*3 : []))
-- === [2*1, 2*2, 2*3]
-- ==> [2,4,6]

-- the direct way of generating a list is simpler, more efficient and more idiomatic. 

-- Tail recursive version
{- 
doubleList :: [Int] -> [Int]
doubleList xs = go [] xs
    where go result [] = result
          go result (x:xs) = go (result++[2*x]) xs
 -}



{- 
  map :: (a -> b) -> [a] -> [b]
  map _ []     = []
  map f (x:xs) = f x : map f xs 
-}

{- 
  filter :: (a -> Bool) -> [a] -> [a]
  filter _pred []    = []
  filter pred (x:xs)
    | pred x         = x : filter pred xs
    | otherwise      = filter pred xs 
-}


-- List Comprehensions

-- these two forms are equivalent:
-- [f x | x <- lis, p x]
-- map f (filter p lis)

-- ghci> [ first ++ " " ++ last | first <- ["John", "Mary"], last <- ["Smith","Cooper"] ]
-- ["John Smith","John Cooper","Mary Smith","Mary Cooper"]


-- ghci> [ reversed | word <- ["this","is","a","string"], let reversed = reverse word ]
-- ["siht","si","a","gnirts"]

 -- pattern matching in list comprehensions!
firstLetters string = [ char | (char:_) <- words string ]

-- ghci> words "Hello World"  //words take a string and split it into [words]
-- ["Hello","World"]
-- ghci> firstLetters "Hello World"
-- "HW"


-- Custom Operators
(<+>) :: [Int] -> [Int] -> [Int]
xs <+> ys = zipWith (+) xs ys
-- ghci> [1,2] <+> [3,4]
-- [4,6]


(+++) :: String -> String -> String
a +++ b = a ++ " " ++ b
-- ghci> "Hello" +++ "World"
-- "Hello World"

-- ghci> "Hello" ++ "World"
-- "HelloWorld" //no space in betwen the two words


keepElements :: [a] -> [Bool] -> [a]
keepElements xs bs = map fst (zip xs bs)
-- ghci> zip [5,6,7,8] [True,False,True,False]
-- [(5,True),(6,False),(7,True),(8,False)]

-- ghci> fst (5,True) -- fst that grabs the first out of a pair. 
-- 5

-- ghci> keepElements [5,6,7,8] [True,False,True,False]
-- [5,6,7,8]

keepElements1 :: [a] -> [Bool] -> [a]
keepElements1 xs bs = map fst (filter snd (zip xs bs))

-- ghci> snd (5,True)  -- snd function grabs the second element out of the tuple
-- True

-- ghci> filter snd [(5,True),(6,True),(7,True),(8,False)]
-- [(5,True),(6,True),(7,True)]

-- ghci> map fst [(5,True),(6,True),(7,True)]
-- [5,6,7]

fsnd (_:x:_) = x  -- Returns the second element of a list

-- ghci> fsnd [1,2,3,4,5]
-- 2

-----------------------------------------------
-- Tuples

-- zip :: [a] -> [b] -> [(a, b)]    -- two lists to list of pairs
-- unzip :: [(a, b)] -> ([a], [b])  -- list of pairs to pair of lists
-- partition :: (a -> Bool) -> [a] -> ([a], [a])    -- elements that satisfy and don't satisfy a predicate


-- ghci> zip [1,2,3] [True,False,True]
-- [(1,True),(2,False),(3,True)]

-- ghci> unzip [(1,True),(2,False),(3,True)]
-- ([1,2,3],[True,False,True])

-- ghci> partition (>0) [-1,1,-4,3,2,0]
-- ([1,3,2],[-1,-4,0])


swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)


-- sum all numbers that are paired with True
sumIf :: [(Bool,Int)] -> Int
sumIf [] = 0
sumIf ((True,x):xs) = x + sumIf xs
sumIf ((False,_):xs) = sumIf xs

-- ghci> sumIf [(True,1),(False,10),(True,100)]
-- 101

{- 
sumNumbers :: [Int] -> Int
sumNumbers [] = 0
sumNumbers (x:xs) = x + sumNumbers xs
 -}


-- sumNumbers xs == foldr (+) 0 xs


 {- 
myMaximum :: [Int] -> Int
myMaximum [] = 0
myMaximum (x:xs) = go x xs
  where go biggest [] = biggest
        go biggest (x:xs) = go (max biggest x) xs
 -}



 {- 
countNothings :: [Maybe a] -> Int
countNothings [] = 0
countNothings (Nothing : xs) = 1 + countNothings xs
countNothings (Just _  : xs) = countNothings xs
 -}

{- 
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f y []     = y
foldr f y (x:xs) = f x (foldr f y xs)
 -}


{- 
f :: (Int -> Int) -> Int -> Bool
f g x = x == g x 
-}


{- f :: (Eq a) => (a -> a) -> a -> Bool  -- type Ea is needed to use operator ==
f g x = x == g x
-}

-- type Ea is needed to use operator ==
-- Prelude> f g x = x == g x
-- Prelude> :type f
-- f :: (Eq a) => (a -> a) -> a -> Bool

-- multiple constraints:
bothPairsEqual :: (Eq a, Eq b) => a -> a -> b -> b -> Bool
bothPairsEqual left1 left2 right1 right2 = left1 == left2 && right1 == right2

-- ghci> bothPairsEqual True True False True
-- False
-- ghci> bothPairsEqual True True False False
-- True
-- ghci> bothPairsEqual 1 1 2 1
-- False
-- ghci> bothPairsEqual 1 1 2 2
-- True

-- ghci> nub [3,5,3,1,1]  -- eliminates duplicates
-- [3,5,1]


-- ghci> compare 1 1
-- EQ
-- ghci> compare 1 3
-- LT
-- ghci> compare 4 3
-- GT

-- ghci> min 1 2 
-- 1

-- ghci> max 1 2
-- 2

-- strings are compared alphabetically
-- ghci> "aardvark" < "banana"   
-- True
-- ghci> "aardvark" < "aanana"
-- False

-- ghci> [2,2,3] > [2,5]
-- False
-- ghci> [3,2,3] > [2,5]
-- True

-- ghci> sort [6,1,4,8,2]
-- [1,2,4,6,8]

-- ghci> sort "black sphinx of quartz, judge my vow!"
-- "      !,aabcdefghijklmnoopqrstuuvwxyz"



-- from the module Data.List
-- sorts a list using the given comparison function
-- sortBy :: (a -> a -> Ordering) -> [a] -> [a]

-- from the module Data.Ord
-- compares two values "through" the function f
comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
comparing f x y = compare (f x) (f y)

-- sorts lists by their length
sortByLength :: [[a]] -> [[a]]
sortByLength = sortBy (comparing length)

-- ghci> sortByLength [[1,2,3],[4,5],[4,5,6,7]]
-- [[4,5],[1,2,3],[4,5,6,7]]

{- 
  (+) :: Num a => a -> a -> a
  (-) :: Num a => a -> a -> a
  (*) :: Num a => a -> a -> a
  negate :: Num a => a -> a    -- 0-x
  abs :: Num a => a -> a       -- absolute value
  signum :: Num a => a -> a    -- -1 for negative values, 0 for 0, +1 for positive values
  fromInteger :: Num a => Integer -> a 
-}

-- ghci> signum 0
-- 0
-- ghci> signum 11
-- 1
-- ghci> signum (-11)
-- -1

-- ghci> negate 1
-- -1
-- ghci> negate (-1)
-- 1

-- ghci> show [3]
-- "[3]"
-- ghci> show [2,3]
-- "[2,3]"
-- ghci> show 3
-- "3"

-- ghci> read "3" :: Int
-- 3
-- ghci> read "3" :: Double 
-- 3.0

{- 
  foldr (+) 1 Nothing   ==> 1
  foldr (+) 1 (Just 3)  ==> 4
  length Nothing        ==> 0
  length (Just 'a')     ==> 1
-}


-- Map
{- 
  -- Create a Map from a list of key-value pairs
  Map.fromList :: Ord k => [(k, a)] -> Map.Map k a

  -- Insert a value into a map. Overrides any previous value with the same key.
  -- Returns a new map. Does not mutate the given map.
  Map.insert :: Ord k => k -> a -> Map.Map k a -> Map.Map k a

  -- Get a value from a map using a key. Returns Nothing if the key was not present in the map.
  Map.lookup :: Ord k => k -> Map.Map k a -> Maybe a

  -- An empty map
  Map.empty :: Map.Map k a
-}

{- 
  ghci> values = Map.fromList [("z",3),("w",4)]
  ghci> Map.lookup "z" values
  Just 3
  ghci> Map.lookup "banana" values
  Nothing
  ghci> Map.insert "x" 7 values
  fromList [("w",4),("x",7),("z",3)]
  ghci> values
  fromList [("w",4),("z",3)] -- original Map not changed, immutability
  ghci> Map.insert "x" 1 (Map.insert "y" 2 values) 
  fromList [("w",4),("x",1),("y",2),("z",3)] -- two insertions
-}

withdraw :: String -> Int -> Map.Map String Int -> Map.Map String Int
withdraw account amount bank =
  case Map.lookup account bank of
    Nothing  -> bank                                   -- account not found, no change
    Just sum -> Map.insert account (sum-amount) bank   -- set new balance


-- ghci> bank = Map.fromList [("Bob",100),("Mike",50)]
-- ghci> withdraw "Bob" 80 bank
-- fromList [("Bob",20),("Mike",50)]

-- ghci> bank
-- fromList [("Bob",100),("Mike",50)] -- note immutability

-- ghci> withdraw "Bozo" 1000 bank --key not found
-- fromList [("Bob",100),("Mike",50)]


withdraw1 :: String -> Int -> Map.Map String Int -> Map.Map String Int
withdraw1 account amount = Map.adjust (\x -> x-amount) account
--withdraw1 account amount bank = Map.adjust (\x -> x-amount) account bank


-- ghci> bank = Map.fromList [("Bob",100),("Mike",50)]
-- ghci> withdraw1 "Bob" 80 bank
-- fromList [("Bob",20),("Mike",50)]
-- ghci> bank
-- fromList [("Bob",100),("Mike",50)]


-- Data.Array
-- array :: Ix i => (i, i) -> [(i, e)] -> Array i e

myArray :: Array Int String
myArray = array (7,11) [(7,"seven"), (8,"eight"), (9,"nine"), (10,"ten"), (11,"ELEVEN")]

-- listArray :: Ix i => (i, i) -> [e] -> Array i e

myArray1 :: Array Int String
myArray1 = listArray (7,11) ["seven", "eight", "nine", "ten", "ELEVEN"]


{- Arrays are used with two new operators:
  Array lookup
  (!) :: Ix i => Array i e -> i -> e
  -- Array update
  (//) :: Ix i => Array i e -> [(i, e)] -> Array i e 
-}

-- ghci> myArray = listArray (7,11) ["seven", "eight", "nine", "ten", "ELEVEN"]
-- ghci> myArray
-- array (7,11) [(7,"seven"),(8,"eight"),(9,"nine"),(10,"ten"),(11,"ELEVEN")]

-- ghci> myArray ! 8          -- get by index/key?
-- "eight"
-- ghci> myArray // [(8,"ocho"),(9,"nueve")]    -- update by index, returns new array
-- array (7,11) [(7,"seven"),(8,"ocho"),(9,"nueve"),(10,"ten"),(11,"ELEVEN")]
-- ghci> myArray
-- array (7,11) [(7,"seven"),(8,"eight"),(9,"nine"),(10,"ten"),(11,"ELEVEN")]  --immutability

-- ghci> length (array (7,11) [(7,"seven"),(8,"eight"),(9,"nine"),(10,"ten"),(11,"ELEVEN")])
-- 5
-- ghci> foldr (+) 0 (Map.fromList [("banana",3),("egg",7)])
-- 10



-- Datatypes

-- definition of a type with three values
data Color = Red | Green | Blue

-- a function that uses pattern matching on our new type
rgb :: Color -> [Double]
rgb Red = [1,0,0]
rgb Green = [0,1,0]
rgb Blue = [0,0,1]

-- ghci> :t Red
-- Red :: Color
-- ghci> :t [Red,Blue,Green]
-- [Red,Blue,Green] :: [Color]
-- ghci> rgb Red
-- [1.0,0.0,0.0]


data Report = ConstructReport Int String String
-- ghci>  :t ConstructReport 1 "Title" "This is the body."
-- ConstructReport 1 "Title" "This is the body." :: Report

reportContent :: Report -> String
reportContent (ConstructReport id title content) = content

setReportContent :: Report -> String -> Report
setReportContent (ConstructReport id title content) ctnt =  ConstructReport id title ctnt
-- setReportContent (ConstructReport id title content) = ConstructReport id title

--  r = ConstructReport 1 "Title" ""
-- ghci> :t (setReportContent r "?")
-- (setReportContent r "?") :: Report
-- ghci> show $ reportContent $ setReportContent r "Body"
-- "\"Body\""


-- to print self-defined types, just add a deriving Show after the type definition:
data Card = Joker | Heart Int | Club Int | Spade Int | Diamond Int  deriving Show

-- ghci> map Heart [1,2,3]
-- [Heart 1,Heart 2,Heart 3]
-- ghci> (Heart . (\x -> x+1)) 1
-- Heart 2


data Described a = Describe a String

getValue :: Described a -> a
getValue (Describe x _) = x

getDescription :: Described a -> String
getDescription (Describe _ desc) = desc

-- ghci> getValue (Describe 3 "a number")
-- 3
-- ghci> getDescription (Describe 3 "a number")
-- "a number"


---------------------------------------------------------
-- data IntList = Empty | Node Int IntList
--   deriving Show

-- ihead :: IntList -> Int
-- ihead (Node i _) = i

-- itail :: IntList -> IntList
-- itail (Node _ t) = t

-- ilength :: IntList -> Int
-- ilength Empty = 0
-- ilength (Node _ t) = 1 + ilength t

-- ghci> ihead (Node 3 (Node 5 (Node 4 Empty)))
-- 3
-- ghci> itail (Node 3 (Node 5 (Node 4 Empty)))
-- Node 5 (Node 4 Empty)
-- ghci> ilength (Node 3 (Node 5 (Node 4 Empty)))
-- 3
---------------------------------------------------------


-- to put any type of element in our list,
data List a = Empty | Node a (List a)
  deriving Show

lhead :: List a -> a
lhead (Node h _) = h

ltail :: List a -> List a
ltail (Node _ t) = t

lnull :: List a -> Bool
lnull Empty = True
lnull _     = False

llength :: List a -> Int
llength Empty = 0
llength (Node _ t) = 1 + llength t

-- ghci> lhead (Node 3 (Node 5 (Node 4 Empty)))
-- 3
-- ghci> ltail  (Node 3 (Node 5 (Node 4 Empty)))
-- Node 5 (Node 4 Empty)
-- ghci> lnull   (Node 3 (Node 5 (Node 4 Empty)))
-- False
-- ghci> length (Node 3 (Node 5 (Node 4 Empty)))
-- ghci> llength (Node 3 (Node 5 (Node 4 Empty)))
-- 3

-- ghci> llength (Node "a" (Node "b" (Node "c" Empty)))
-- 3
-- ghci> lhead  (Node "a" (Node "b" (Node "c" Empty)))
-- "a"
-- ghci> ltail  (Node "a" (Node "b" (Node "c" Empty)))
-- Node "b" (Node "c" Empty)
-- ghci> lnull Empty
-- True


---------------------------------------------------------
--Tree
---------------------------------------------------------
-- data Tree a = Node a (Tree a) (Tree a) | Empty
-- example :: Tree Int
-- example = (Node 0 (Node 1 (Node 2 Empty Empty)
--                           (Node 3 Empty Empty))
--                   (Node 4 Empty Empty))

-- treeHeight :: Tree a -> Int
-- treeHeight Empty = 0
-- treeHeight (Node _ l r) = 1 + max (treeHeight l) (treeHeight r)

-- treeHeight Empty ==> 0
-- treeHeight (Node 2 Empty Empty)
--   ==> 1 + max (treeHeight Empty) (treeHeight Empty)
--   ==> 1 + max 0 0
--   ==> 1
-- treeHeight (Node 1 Empty (Node 2 Empty Empty))
--   ==> 1 + max (treeHeight Empty) (treeHeight (Node 2 Empty Empty))
--   ==> 1 + max 0 1
--   ==> 2
-- treeHeight (Node 0 (Node 1 Empty (Node 2 Empty Empty)) Empty)
--   ==> 1 + max (treeHeight (Node 1 Empty (Node 2 Empty Empty))) (treeHeight Empty)
--   ==> 1 + max 2 0
--   ==> 3
---------------------------------------------------------

-- data Person = MkPerson String Int String String String deriving Show
data Person = MkPerson { name :: String, age :: Int, town :: String, state :: String, profession :: String}
  deriving Show

-- ghci> MkPerson "Jane"  21 "Espoo" "Espoo" "Teacher" 
-- MkPerson {name = "Jane", age = 21, town = "Espoo", state = "Espoo", profession = "Teacher"} 

-- ghci> MkPerson {name = "Jane Doe", town = "Houston", profession = "Engineer", state = "Texas", age = 21}
-- MkPerson {name = "Jane Doe", age = 21, town = "Houston", state = "Texas", profession = "Engineer"}


people :: [Person]
people = [ MkPerson "Jane Doe" 21 "Houston" "Texas" "Engineer"
         , MkPerson "Maija Meiken" 35 "Rovaniemi" "Finland" "Engineer"
         , MkPerson "Mauno Mutikainen" 27 "Turku" "Finland" "Mathematician"
         ]

query :: [Person] -> [Person]
query []     = []
query (x:xs)
  | state x == "Finland" && profession x == "Engineer" =
      x : query xs
  | otherwise = query xs

-- ghci> query people 
-- [MkPerson {name = "Maija Meiken", age = 35, town = "Rovaniemi", state = "Finland", profession = "Engineer"}]


-----------------------------
--Part 6--
-----------------------------
-- class Size a where
--   size :: a -> Int

-- instance Size Int where
--   size x = abs x

-- instance Size [a] where
--   size xs = length xs

-- ghci> :t size 
-- size :: Size a => a -> Int
-- ghci> size [1,2]
-- 2
-- ghci> size [""]
-- 1

-- sizeBoth :: (Size a1, Size a2) => a1 -> a2 -> [Int]
-- sizeBoth a b = [size a, size b]
-- ghci> :t sizeBoth
-- sizeBoth :: (Size a1, Size a2) => a1 -> a2 -> [Int]


class Size a where
  empty :: a
  size :: a -> Int
  sameSize :: a -> a -> Bool

instance Size (Maybe a) where
  empty = Nothing

  size Nothing = 0
  size (Just a) = 1

  sameSize x y = size x == size y

instance Size [a] where
  empty = []
  size xs = length xs
  sameSize x y = size x == size y