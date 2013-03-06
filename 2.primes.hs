square :: Num a => a -> a
square x = x * x

divisible :: Integral a => a -> a -> Bool
divisible x y = x `rem` y == 0

primeDivisors :: Int -> [Int]
primeDivisors x = filter (divisible x) $ takeWhile (lessThanSqrt x) primes
  where
    lessThanSqrt x = (<= x) . square

prime :: Int -> Bool
prime = null . primeDivisors

primes :: [Int]
primes = 2 : filter prime [3,5..]

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = print $ sum $ primeDivisors $ x + 1
  where
    x = head $ dropWhile (not . prime) $ dropWhile (<= 227000) fibs