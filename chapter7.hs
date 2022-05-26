import Data.Char

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

-- exercise 7.6

type Bit = Int

unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

int2bin :: Int -> [Bit]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null ((take 8) . (++ (repeat 0))) (drop 8)

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold null (f . head) tail

never _ = False

iterate'' :: (a -> a) -> a -> [a]
iterate'' f = unfold never id f

-- exercise 7.7

bin2int :: [Bit] -> Int
bin2int = foldr (\bit acc -> 2 * acc + bit) 0

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

--

addParity :: [Bit] -> [Bit]
addParity bits = sum bits `mod` 2 : bits

encode' :: String -> [Bit]
encode' = concat . map (addParity . make8 . int2bin . ord)

checkParity :: [Bit] -> [Bit]
checkParity (parity : bits) =
  if sum bits `mod` 2 == parity then bits else error "parity error"

chop9 :: [Bit] -> [[Bit]]
chop9 = unfold null ((take 9) . (++ (repeat 0))) (drop 9)

decode' :: [Bit] -> String
decode' = map (chr . bin2int . checkParity) . chop9

transmit :: ([Bit] -> [Bit]) -> String -> String
transmit channel = decode . channel . encode

transmit' :: ([Bit] -> [Bit]) -> String -> String
transmit' channel = decode' . channel . encode'

-- exercise 7.9

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x : xs) = f x : altMap g f xs

-- exercise 7.10

luhnDouble :: Int -> Int
luhnDouble x = if y > 9 then y - 9 else y where y = 2 * x

luhn :: [Int] -> Bool
luhn = (== 0) . (`mod` 10) . sum . altMap id luhnDouble . reverse
