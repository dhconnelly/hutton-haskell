-- exercise 6.1

fact :: Int -> Int
fact n
  | n <= 1 = 1
  | otherwise = n * fact (n - 1)

-- exercise 6.2

sumdown :: Int -> Int
sumdown n
  | n <= 0 = 0
  | otherwise = n + sumdown (n - 1)

-- exercise 6.3

(⊗) :: Int -> Int -> Int
x ⊗ 0 = 1
x ⊗ y = x * (x ⊗ (y - 1))

-- exercise 6.4

euclid :: Int -> Int -> Int
euclid x y
  | x == y = x
  | x < y = euclid x (y - x)
  | otherwise = euclid (x - y) y

-- exercise 6.5

-- length [1, 2, 3] = 1 + length [2, 3] = 1 + 1 + length [3] = 1 + 1 + 1 = 3
-- drop 3 [1, 2, 3, 4, 5] = drop 2 [2, 3, 4, 5] = drop 1 [3, 4, 5] = drop 0 [4, 5] = [4, 5]
-- init [1, 2, 3] = 1 : init [2, 3] = 1 : 2 : init [3] = 1 : 2 : [] = [1, 2]

-- exercise 6.6

and' :: [Bool] -> Bool
and' [] = True
and' (True : xs) = and' xs
and' (False : xs) = False

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs : xss) = xs ++ concat' xss

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

(!!!) :: [a] -> Int -> a
(x : xs) !!! 0 = x
(x : xs) !!! n = xs !!! (n - 1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y : ys)
  | x == y = True
  | otherwise = elem' x ys

-- exercise 6.7

merge' :: Ord a => [a] -> [a] -> [a]
merge' xs [] = xs
merge' [] ys = ys
merge' (x : xs) (y : ys)
  | x <= y = x : merge' xs (y : ys)
  | otherwise = y : merge' (x : xs) ys

-- exercise 6.8

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge' (msort xs1) (msort xs2)
  where
    (xs1, xs2) = halve xs
    halve ys = (take n ys, drop n ys) where n = div (length ys) 2

-- exercise 6.9

sum' :: [Int] -> Int
sum' [] = 0
sum' (x : xs) = x + sum' xs

take' :: Int -> [Int] -> [Int]
take' _ [] = []
take' n (x : xs) = x : take (n - 1) xs

last' :: [a] -> a
last' [x] = x
last' (x : xs) = last' xs
