-- exercise 7.1

fIfP :: (a -> b) -> (a -> Bool) -> [a] -> [b]
fIfP f p xs = [f x | x <- xs, p x]

fIfP' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
fIfP' f p xs = map f (filter p xs)

-- exercise 7.2

all' :: (a -> Bool) -> [a] -> Bool
all' p xs = length (filter p xs) == length xs

any' :: (a -> Bool) -> [a] -> Bool
any' p xs = not (null (filter p xs))

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x : xs) | p x = x : takeWhile' p xs
takeWhile' p (x : xs) = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x : xs) | p x = dropWhile' p xs
dropWhile' p (x : xs) = xs

-- exercise 7.3

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x ys -> f x : ys) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x ys -> if p x then x : ys else ys) []

-- exercise 7.4

dec2int :: [Int] -> Int
dec2int [] = 0
dec2int xs = foldl (\acc x -> 10 * acc + x) 0 xs

-- exercise 7.5

curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f = \x -> \y -> f (x, y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = \(x, y) -> f x y
