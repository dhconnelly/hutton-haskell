-- exercise 5.1

squares = [i ^ 2 | i <- [1 .. 100]]

-- exercise 5.2

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(i, j) | i <- [0 .. m], j <- [0 .. n]]

-- exercise 5.3

square :: Int -> [(Int, Int)]
square n = [(i, j) | (i, j) <- grid n n, i /= j]

-- exercise 5.4

replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1 .. n]]

-- exercise 5.5

pyths :: Int -> [(Int, Int, Int)]
pyths max =
  [ (x, y, z)
    | x <- [1 .. max],
      y <- [1 .. max],
      z <- [1 .. max],
      x ^ 2 + y ^ 2 == z ^ 2
  ]

-- exercise 5.6

factors :: Int -> [Int]
factors x = [n | n <- [1 .. x], x `mod` n == 0]

perfects :: Int -> [Int]
perfects max = [x | x <- [1 .. max], sum [m | m <- factors x, m /= x] == x]

-- exercise 5.7

concats = concat [[(x, y) | x <- [1, 2]] | y <- [3, 4]]

-- exercise 5.8

find :: Eq a => a -> [(a, b)] -> [b]
find k kvs = [v | (k', v) <- kvs, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0 ..])

-- exercise 5.9

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]
