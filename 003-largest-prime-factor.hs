main :: IO ()
main = do
  putStrLn $ show $ solution

solution :: Int
solution = last $ filter prime $ oddFactors 600851475143

oddFactors :: Int -> [Int]
oddFactors n = [x | x <- [1,3..nSqrt], n `mod` x == 0]
  where nSqrt = ceiling $ sqrt $ fromIntegral n

prime :: Int -> Bool
prime n = oddFactors n == [1]
