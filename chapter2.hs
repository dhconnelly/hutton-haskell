-- exercise 2.2

-- (2^3)*4
-- (2*3)+(4*5)
-- 2+(3*(4^5))

-- exercise 2.3

n = a `div` length xs
  where
    a = 10
    xs = [1, 2, 3, 4, 5]

-- exercise 2.4

last1 xs = head (reverse xs)

last2 xs = xs !! (length xs - 1)

-- exercise 2.5

init1 xs = reverse (tail (reverse xs))

init2 xs = take (length xs - 1) xs
