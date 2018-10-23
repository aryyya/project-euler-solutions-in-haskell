main :: IO ()
main = do
  putStrLn $ show $ solution

solution :: Int
solution = maximum [z | x <- range, y <- range, let z = x * y, palindrome z]

range :: [Int]
range = [100..999]

palindrome :: Int -> Bool
palindrome n = nDigits == reverse nDigits
  where nDigits = digits n

digits :: Int -> [Int]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]
