module Main where
import Web.Secretary 
import System.Environment

main :: IO ()
main = do
  args <- getArgs 
  case args of
    [] -> secretary
    "auth" : _ -> auth
    "show" : _ -> printSchedules
