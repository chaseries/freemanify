import System.Environment (getArgs)
import Data.Char (toUpper, toLower)

-- Version 1

freemanifyHelp :: Bool -> String -> String
freemanifyHelp b (x:xs) =
  case xs of
    [] -> [x]
    _ ->
      case b of
        True -> [toUpper x] ++ freemanifyHelp (not b) xs
        False -> [toLower x] ++ freemanifyHelp (not b) xs


-- Version 2

freemanifyHelp2 :: Bool -> String -> String
freemanifyHelp2 _ (x:[]) = [x]
freemanifyHelp2 True (x:xs) = [toUpper x] ++ freemanifyHelp2 False xs
freemanifyHelp2 False (x:xs) = [toLower x] ++ freemanifyHelp2 True xs


-- Version 3

mapToN :: Int -> Int -> (a -> a) -> [a] -> [a]
mapToN n o f = zipWith ($) (setOff o . drop 1 . cycle . take n $ f : repeat id)
  where setOff o | o < 0 = drop (abs o)
                 | otherwise = ((replicate o id) ++)

mapToOdd :: (a -> a) -> [a] -> [a]
mapToOdd = mapToN 2 (-1)

mapToEven :: (a -> a) -> [a] -> [a]
mapToEven = mapToN 2 0

freemanifyHelp3 :: String -> String
freemanifyHelp3 = (mapToOdd toLower) . (mapToEven toUpper)

-- version 4

infixl 0 <|
(<|) = flip ($)

infixr 0 |>
(|>) = ($)

infixl 9 ~>
(~>) = flip (.)

infixr 9 <~
(<~) = (.)

mapToN' n o f = zipWith ($) (take n ~> cycle ~> drop 1 ~> setOff o |> f : repeat id)
  where setOff o | o < 0 = drop (abs o)
                 | otherwise = ((replicate o id) ++)

mapToOdd' = mapToN' 2 (-1)

mapToEven' = mapToN' 2 0

freemanifyHelp4 = (mapToOdd' toLower) ~> (mapToEven' toUpper)

-- Final
freemanify :: String -> String
freemanify = freemanifyHelp4




main = do
  input <- getArgs
  mapM (putStrLn . freemanify) input
