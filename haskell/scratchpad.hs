add                     :: Integer -> Integer -> Integer
add x y                 =  x + y

inc                    = add 1


describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list."  

describeList1 :: [a] -> String  
describeList1 xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."                                                

-- Compute the sum of the integers from 1 to n.
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3*n + 1


foo :: Integer -> Integer
foo 0 = 16
foo 1 
  | "Haskell" > "C++" = 3
  | otherwise         = 4
foo n
  | n < 0            = 0
  | n `mod` 17 == 2  = -43
  | otherwise        = n + 3

sumPair :: (Int,Int) -> Int
sumPair (x,y) = x + y+10


f :: Int -> Int -> Int -> Int
f x y z = x + y + z
ex17 = f 3 17 8


data Thing = Shoe 
           | Ship 
           | SealingWax 
           | Cabbage 
           | King
  deriving Show


shoe :: Thing
shoe = Shoe


listO'Things :: [Thing]
listO'Things = [Shoe, SealingWax, King, Cabbage, King]

capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]  

first :: String -> String  
first "" = "Empty string, whoops!"  
first all@(x:xs) = [x]  


isSmall :: Thing -> Bool
isSmall Shoe       = True
isSmall Ship       = False
isSmall SealingWax = True
isSmall Cabbage    = True
isSmall King       = False

initials :: String -> String -> String
initials firstname lastname 
	| firstname == "brian" = [f] ++ ". " ++ [l] ++ "."
	| firstname == "bob" = first firstname ++ ". " ++ first lastname ++ "." ++ jim
	| otherwise = "not valid input"
    where (f:_) = firstname  
          (l:_) = lastname  
	  jim = "jimmy"

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2  


quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 

quicksortr :: (Ord a) => [a] -> [a]  
quicksortr [] = []  
quicksortr (x:xs) =   
    let smallerSorted = quicksortr [a | a <- xs, a <= x]  
        biggerSorted = quicksortr [a | a <- xs, a > x]  
    in  biggerSorted ++ [x] ++  smallerSorted

multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z  

compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred x = compare 100 x 

idd :: a -> a
idd x = x

allEqual :: Eq(a) => a -> a -> a -> Bool
allEqual x y z = x == y && y == z

applyAndConcat :: (String -> String) -> (String -> String) -> String -> String
applyAndConcat f g x = (f x) ++ (g x)

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g = (\x -> f (g x)) 
-- alternatively
--f . g = (\x -> f (g x))

applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)  

chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1)   

numLongChains1 :: Int  
numLongChains1 = length (filter isLong (map chain [1..100]))  
    where isLong xs = length xs > 15  

numLongChains2 :: Int  
numLongChains2 = length (filter (\xs -> length xs > 15) (map chain [1..100]))  

flip1 :: (a -> b -> c) -> b -> a -> c  
flip1 f = \x y -> f y x  

sum' :: (Num a) => [a] -> a  
sum' xs = foldl (\acc x -> acc + x) 0 xs  

elem' :: (Eq a) => a -> [a] -> Bool  
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys  

maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x > acc then x else acc)  
  
reverse' :: [a] -> [a]  
reverse' = foldl (\acc  x -> x : acc) []  
  
product' :: (Num a) => [a] -> a  
product' = foldr1 (*)  
  
filter' :: (a -> Bool) -> [a] -> [a]  
filter' p = foldr (\x acc -> if p x then x : acc else acc) []  
  
head' :: [a] -> a  
head' = foldr1 (\x _ -> x)  
  
last' :: [a] -> a  
last' = foldl1 (\_ x -> x) 

head' :: [a] -> a  
head' [] = error "No head for empty lists!"  
head' (x:_) = x  


--temp work
--fib :: (Integral a) => [a] -> [a]
fib [] = fib ([1] :2)
fib xs = xs ++ ((xs!!((length xs)-1)) + (xs!!((length xs)-2)): [])



