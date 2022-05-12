-- exercise 4.1

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

-- exercise 4.2

third1 :: [a] -> a
third1 xs = head (tail (tail xs))

third2 :: [a] -> a
third2 xs = xs !! 2

third3 :: [a] -> a
third3 (_ : _ : x : _) = x

-- exercise 4.3

safetail1 :: [a] -> [a]
safetail1 xs = if null xs then [] else tail xs

safetail2 :: [a] -> [a]
safetail2 xs
  | null xs = []
  | otherwise = tail xs

safetail3 :: [a] -> [a]
safetail3 (_ : xs) = xs
safetail3 _ = []

-- exercise 4.4

(#||) :: Bool -> Bool -> Bool
True #|| True = True
True #|| False = True
False #|| True = True
False #|| False = False

(##||) :: Bool -> Bool -> Bool
True ##|| _ = True
_ ##|| True = True
_ ##|| _ = False

(###||) :: Bool -> Bool -> Bool
True ###|| _ = True
False ###|| x = x

(####||) :: Bool -> Bool -> Bool
x ####|| y
  | x = True
  | y = True
  | otherwise = False

--- exercise 4.5

(#&&) :: Bool -> Bool -> Bool
x #&& y =
  if x
    then
      if y
        then True
        else False
    else False

--- exercise 4.6

(##&&) :: Bool -> Bool -> Bool
x ##&& y = if x then y else False

--- exercise 4.7

mult :: Int -> Int -> Int -> Int
mult = \x -> \y -> \z -> x * y * z
