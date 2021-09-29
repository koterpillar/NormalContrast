module Utils where

lpad :: Int -> String -> String
lpad n = reverse . rpad n . reverse

rpad :: Int -> String -> String
rpad n []     = replicate n ' '
rpad n (x:xs) = x : rpad (n - 1) xs
