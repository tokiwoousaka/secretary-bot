{-# LANGUAGE DataKinds #-}
module Main where
import Web.Twitter.Conduit
import Web.Twitter.Murmur 
import Control.Monad.IO.Class
import Options.Declarative
import System.Directory

main :: IO ()
main = do
  putStrLn "Hello"
  -- auth
  -- twInfo <- readTWInfo
  -- print twInfo
  -- getMentionsTimeline twInfo 10 >>= print

readTWInfo :: IO TWInfo
readTWInfo = twInfoFileName >>= readFile >>= return . read

auth :: IO ()
auth = do
  twInfo <- getTWInfo murAuth
  twInfoFileName >>= flip writeFile (show twInfo)

----


twInfoFileName :: IO String
twInfoFileName = do
  homeDir <- getHomeDirectory
  createDirectoryIfMissing False $ homeDir ++ "/.secretary-bot"
  return $ homeDir ++ "/.secretary-bot/twInfo"

murAuth :: MurmurAuth 
murAuth = MurmurAuth
  { murConsumerKey= consumerKey
  , murConsumerSecret = consumerSecret
  , murGetPINAction = \url -> do
      putStrLn $ "Access '" ++ url ++ "' for Accept. And type PIN code here."
      getLine
  }

consumerKey :: String
consumerKey = ""

consumerSecret :: String
consumerSecret = ""

