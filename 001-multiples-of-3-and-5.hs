main :: IO ()
main = do
  putStrLn $ show $ solution

solution :: Int
solution = sum $ filter (\n -> n `divBy` 3 || n `divBy` 5) [1..999]

divBy :: Int -> Int -> Bool
divBy x y = x `mod` y == 0
