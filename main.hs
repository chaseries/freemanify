import System.Environment (getArgs)
import Data.Char (toUpper, toLower)

freemanifyHelp :: Bool -> String -> String
freemanifyHelp b (x:xs) =
  case xs of
    [] -> [x]
    _ ->
      case b of
        True -> [toUpper x] ++ freemanifyHelp (not b) xs
        False -> [toLower x] ++ freemanifyHelp (not b) xs

freemanify :: String -> String
freemanify = freemanifyHelp False

main = do
  input <- getArgs
  mapM (putStrLn . freemanify) input
