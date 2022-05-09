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
