{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Web.Twitter.Conduit (TWInfo)
import Web.Secretary.Twitter
import Web.Secretary.Parser

import Data.Time
import Data.List
import Control.Monad.IO.Class
import System.Directory
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe
import Text.Read

main :: IO ()
main = secretary

secretary :: IO ()
secretary = do
  twInfo <- readTWInfo
  tl <- do
    t <- getMentionsTimeline twInfo 10 
    return $ sortBy (\l r -> twStatusId l `compare` twStatusId r) t
  targets <- case tl of
    [] -> return []
    xs -> do
      v <- loadTlInfo
      putStrLn " --- got tweets"
      saveTlInfo . twStatusId . head $ reverse xs
      return . filter ((v <) . twStatusId) $ xs
  putStrLn " --- target tweets"
  sequenceMap_ printTweet $ targets
  putStrLn " --- run schedule"
  runSecretary $ map twText targets

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
-- スケジュール管理

maybeTargetSchedule :: (Schedule, Maybe LocalTime) -> LocalTime -> Maybe Schedule
maybeTargetSchedule (_, Just _) _ = Nothing --TODO: 繰り返しスケジュール
maybeTargetSchedule (sc@(ScheduleNow s), Nothing) _ = Just sc
maybeTargetSchedule (sc@(ScheduleLocalTime lt _), Nothing) now 
  = if lt < now then Just sc else Nothing

isOldSchedule :: (Schedule, Maybe LocalTime) -> Bool
isOldSchedule (_, Just _) = False
isOldSchedule (sc@(ScheduleNow _), Nothing) = True
isOldSchedule (sc@(ScheduleLocalTime _ _), Nothing) = True

printSchedule :: (Schedule, Maybe LocalTime) -> IO ()
printSchedule ((ScheduleNow s), la) = putStrLn $ "ただちに発言 : '" ++ s ++ "' 最終発言日時 : " ++ show la
printSchedule ((ScheduleLocalTime lt s), la) = putStrLn $ show lt ++ "に発言 : '" ++ s ++ "' 最終発言日時 : " ++ show la
printSchedule s = putStrLn $ "出力未対応 : " ++ show s

----
-- 実行

runSecretary :: [T.Text] -> IO ()
runSecretary texts = do
    twInfo <- readTWInfo
    fs <- loadScheduleInfo
    let sc = concatMap (maybeListToList . parseCommand) $ texts
    let schedule = initSchedule sc ++ fs
    res <- sequenceMap (runCommand twInfo) schedule 
    saveScheduleInfo $ filter isOldSchedule res
  where 
    initSchedule :: [Schedule] -> [(Schedule, Maybe LocalTime)]
    initSchedule = map (flip (,) Nothing)

runCommand :: TWInfo -> (Schedule, Maybe LocalTime) -> IO (Schedule, Maybe LocalTime)
runCommand tw st = do 
    now <- getNow
    printSchedule st
    case maybeTargetSchedule st now of
      Just sc -> do
        putStrLn "   -> Run"
        run sc >> return (sc, Just now)
      Nothing -> do
        return st
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

----
-- Util

sequenceMap_ :: (Foldable f, Functor f, Monad m) => (a -> m b) -> f a -> m ()
sequenceMap_ f = sequence_  . fmap f

sequenceMap :: (Traversable f, Functor f, Monad m) => (a -> m b) -> f a -> m (f b)
sequenceMap f = sequence  . fmap f

maybeListToList :: Maybe [a] -> [a]
maybeListToList (Just xs) = xs
maybeListToList Nothing = []

getNow :: IO LocalTime
getNow = do
  utc <- getZonedTime >>= return . zonedTimeToUTC
  timeZone <- getCurrentTimeZone
  return $ utcToLocalTime timeZone utc
