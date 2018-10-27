main :: IO ()
main = do
  putStrLn $ show $ solution

solution :: Int
solution = last $ take 10001 primes

oddFactors :: Int -> [Int]
oddFactors n = 1 : [f | f <- [3,5..nSqrt], n `mod` f == 0] ++ [n]
  where nSqrt = ceiling $ sqrt $ fromIntegral n

prime :: Int -> Bool
prime 1 = False
prime 2 = True
prime n
  | n `mod` 2 == 0 = False
  | otherwise      = oddFactors n == [1, n]

primes :: [Int]
primes = 2 : [n | n <- [3,5..], prime n]
