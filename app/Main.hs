{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Web.Twitter.Conduit (TWInfo)
import Web.Twitter.Murmur 
import Web.Secretary.Parser

import Data.Time
import Data.List
import Control.Monad.IO.Class
import Options.Declarative
import System.Directory
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe
import Text.Read

main :: IO ()
main = do
  twInfo <- readTWInfo
  tl <- do
    t <- getMentionsTimeline twInfo 10 
    return $ sortBy (\l r -> twStatusId l `compare` twStatusId r) t
  case tl of
    [] -> return ()
    xs -> do
      v <- loadTlInfo
      putStrLn " --- got tweets"
      sequenceMap_ printTweet $ xs
      let targes = filter ((v <) . twStatusId) $ xs
      putStrLn " --- target tweets"
      sequenceMap_ (runSchedules . twText) $ targes
      sequenceMap_ printTweet $ targes
      saveTlInfo . twStatusId . head $ reverse xs

readTWInfo :: IO TWInfo
readTWInfo = twInfoFileName >>= readFile >>= return . read

auth :: IO ()
auth = do
  twInfo <- getTWInfo murAuth
  twInfoFileName >>= flip writeFile (show twInfo)

printTweet :: TweetInfo -> IO ()
printTweet tw = T.putStrLn . T.concat $ 
  [ T.pack . show $ twStatusId tw
  , "【"
  , twUserName tw
  , "("
  , twUserScreenName tw
  , ")】"
  , twText tw
  ]

----
-- スケジュール判定

maybeTargetSchedule :: (Schedule, Maybe LocalTime) -> LocalTime -> Maybe Schedule
maybeTargetSchedule (_, Just _) _ = Nothing --TODO: 繰り返しスケジュール
maybeTargetSchedule (sc@(ScheduleNow s), Nothing) _ = Just sc
maybeTargetSchedule (sc@(ScheduleLocalTime lt _), Nothing) now 
  = if lt < now then Just sc else Nothing

----
-- 実行

runSchedules :: T.Text -> IO ()
runSchedules text = do
    twInfo <- readTWInfo --TODO : 読むとこ変える
    fs <- loadScheduleInfo --TODO : 読むとこ変える
    let sc = maybeListToList $ parseCommand text 
    let schedule = initSchedule sc ++ fs
    sequenceMap_ (runCommand twInfo) schedule 
  where 
    initSchedule :: [Schedule] -> [(Schedule, Maybe LocalTime)]
    initSchedule = map (flip (,) Nothing)

runCommand :: TWInfo -> (Schedule, Maybe LocalTime) -> IO (Schedule, Maybe LocalTime)
runCommand tw st = do 
    now <- do
      utc <- getZonedTime >>= return . zonedTimeToUTC
      timeZone <- getCurrentTimeZone
      return $ utcToLocalTime timeZone utc
    case maybeTargetSchedule st now of
      Just sc -> run sc >> return (sc, Just now)
      Nothing -> return st
  where
    run :: Schedule -> IO ()
    run (ScheduleNow str) = postTweet tw $ "@its_out_of_tune " ++ str
    run (ScheduleLocalTime _ str) = postTweet tw $ "@its_out_of_tune " ++ str

----
-- ファイル管理

twInfoFileName :: IO String
twInfoFileName = do
  homeDir <- getHomeDirectory
  createDirectoryIfMissing False $ homeDir ++ "/.secretary-bot"
  return $ homeDir ++ "/.secretary-bot/twInfo"

tlInfoFileName :: IO String
tlInfoFileName = do
  homeDir <- getHomeDirectory
  createDirectoryIfMissing False $ homeDir ++ "/.secretary-bot"
  return $ homeDir ++ "/.secretary-bot/tlInfo"

scheduleInfoFileName :: IO String
scheduleInfoFileName = do
  homeDir <- getHomeDirectory
  createDirectoryIfMissing False $ homeDir ++ "/.secretary-bot"
  return $ homeDir ++ "/.secretary-bot/schedule"

loadScheduleInfo :: IO [(Schedule, Maybe LocalTime)]
loadScheduleInfo = scheduleInfoFileName >>= loadFileInfo []

loadTlInfo :: IO Integer
loadTlInfo = tlInfoFileName >>= loadFileInfo 0

loadFileInfo :: Read a => a -> String -> IO a
loadFileInfo def fn = do
  exist <- doesFileExist fn
  if exist 
    then do
      ti <- readFile fn
      case readMaybe ti of
        Just s -> return s
        _ -> return def
    else
      return def

saveScheduleInfo :: [(Schedule, Maybe LocalTime)] -> IO ()
saveScheduleInfo v = scheduleInfoFileName >>= saveFileInfo v

saveTlInfo :: Integer -> IO ()
saveTlInfo v = tlInfoFileName >>= saveFileInfo v

saveFileInfo :: Show a => a -> String -> IO ()
saveFileInfo v fn = writeFile fn $ show v

----
-- 認証系

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

sequenceMap_ :: (Foldable f, Functor f, Monad m) => (a -> m b) -> f a -> m ()
sequenceMap_ f = sequence_  . fmap f

maybeListToList :: Maybe [a] -> [a]
maybeListToList (Just xs) = xs
maybeListToList Nothing = []
