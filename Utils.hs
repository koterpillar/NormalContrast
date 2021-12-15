module Utils where

lpad' :: a -> Int -> [a] -> [a]
lpad' c n = reverse . rpad' c n . reverse

lpad :: Int -> String -> String
lpad = lpad' ' '

rpad' :: a -> Int -> [a] -> [a]
rpad' c n []     = replicate n c
rpad' c n (x:xs) = x : rpad' c (n - 1) xs

rpad :: Int -> String -> String
rpad = rpad' ' '
