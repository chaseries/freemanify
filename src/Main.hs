import System.Environment (getArgs)
import Data.Char (toUpper, toLower)


mapToN :: Int -> Int -> (a -> a) -> [a] -> [a]
mapToN n o f = zipWith ($) (setOff o . drop 1 . cycle . take n $ f : repeat id)
  where setOff o | o < 0 = drop (abs o)
                 | otherwise = ((replicate o id) ++)

mapToOdd :: (a -> a) -> [a] -> [a]
mapToOdd = mapToN 2 (-1)

mapToEven :: (a -> a) -> [a] -> [a]
mapToEven = mapToN 2 0

freemanify :: String -> String
freemanify = (mapToOdd toLower) . (mapToEven toUpper)


main = do
  input <- getArgs
  mapM (putStrLn . freemanify) input
