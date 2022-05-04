-- exercise 3.1

-- ['a', 'b', 'c'] :: [Char]
-- ('a', 'b', 'c') :: (Char, Char, Char)
-- [(False, '0'), (True, '1')] :: [(Bool, Char)]
-- ([False, True], ['0', '1']) :: ([Bool], [Char])
-- [tail, init, reverse] :: [[a] -> [a]]

-- exercise 3.2

bools :: [Bool]
bools = [True, False]

nums :: [[Int]]
nums = [[i] | i <- [1 .. 10]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f x = f x

-- exercise 3.3

second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- exercise 3.5

-- in order to compare two functions for equality you must determine whether
-- they produce the same results for every possible input, but this requires
-- program evaluation and so cannot be done in the type system prior to
-- execution (i.e. it's part of dynamic semantics and not static semantics).
