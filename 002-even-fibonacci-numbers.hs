main :: IO ()
main = do
  putStrLn $ show $ solution

solution :: Int
solution = sum $ filter even $ fibsUntil 4000000 starterFibs

starterFibs :: [Int]
starterFibs = [2, 1]

nextFib :: [Int] -> [Int]
nextFib fibs = [sum $ take 2 fibs] ++ fibs

nextFibs :: Int -> [Int] -> [Int]
nextFibs 0     fibs = fibs
nextFibs count fibs = nextFibs (count - 1) $ nextFib fibs

fibsUntil :: Int -> [Int] -> [Int]
fibsUntil max fibs
  | (head $ nextFibs) < max = fibsUntil max $ nextFib fibs
  | otherwise               = fibs
  where nextFibs = nextFib fibs
