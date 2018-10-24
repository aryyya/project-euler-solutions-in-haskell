main :: IO ()
main = do
  putStrLn $ show $ solution

solution :: Int
solution = head [n | n <- [20,40..], divByRange n [3..19]]

divByRange :: Int -> [Int] -> Bool
divByRange n r = length (takeWhile (\d -> n `mod` d == 0) r) == length r
