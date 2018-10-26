main :: IO ()
main = do
  putStrLn $ show $ solution

solution :: Int
solution = squareSum [1..100] - sumSquares [1..100]

sumSquares :: [Int] -> Int
sumSquares ns = sum $ [n^2 | n <- ns]

squareSum :: [Int] -> Int
squareSum ns = (sum ns)^2
