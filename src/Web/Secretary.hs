{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Secretary where
import Web.Twitter.Conduit (TWInfo)
import Web.Secretary.ScheduleConstructor
import Web.Secretary.Twitter
import Web.Secretary.File 
  ( loadScheduleInfo 
  , loadTlInfo 
  , saveScheduleInfo 
  , saveTlInfo 
  , readTWInfo
  , writeTWInfo
  )
import Data.Time
import Data.List
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe

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
      saveTlInfo . twStatusId . head $ reverse xs
      return . filter ((/="its_out_of_tune") . twUserName) . filter ((v <) . twStatusId) $ xs
  putStrLn " --- target tweets"
  mapM_ printTweet $ targets
  putStrLn " --- run schedule"
  runSecretary $ map twText targets

auth :: IO ()
auth = do
  twInfo <- getTWInfo murAuth
  writeTWInfo twInfo

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
maybeTargetSchedule (sc@(SchedulePlans _ _), Nothing) _ = Just sc
maybeTargetSchedule (sc@(ScheduleLocalTime lt _), Nothing) now 
  = if lt < now then Just sc else Nothing

isOldSchedule :: (Schedule, Maybe LocalTime) -> Bool
isOldSchedule (_, Just _) = False
isOldSchedule (sc@(ScheduleNow _), Nothing) = True
isOldSchedule (sc@(ScheduleLocalTime _ _), Nothing) = True
isOldSchedule (sc@(SchedulePlans _ _), Nothing) = True

printSchedules :: IO ()
printSchedules = do
  schedules <- loadScheduleInfo
  mapM_ printSchedule schedules

printSchedule :: (Schedule, Maybe LocalTime) -> IO ()
printSchedule ((ScheduleNow s), la) = putStrLn $ "ただちに発言 : '" ++ s ++ "' 最終発言日時 : " ++ show la
printSchedule ((ScheduleLocalTime lt s), la) 
  = putStrLn $ show lt ++ "に発言 : '" ++ s ++ "' 最終発言日時 : " ++ show la
printSchedule ((SchedulePlans lt s), la) 
  = putStrLn $ "発言予定通知 : " ++ show lt ++ "に発言予定 : '" ++ s ++ "' 最終発言日時 : " ++ show la
printSchedule s = putStrLn $ "出力未対応 : " ++ show s

----
-- 実行

runSecretary :: [T.Text] -> IO ()
runSecretary texts = do
    now <- getNow
    putStrLn $ " --- start run secretary : " ++ show now
    twInfo <- readTWInfo
    fs <- loadScheduleInfo
    let sc = concatMap (maybeListToList . parseCommand now) $ texts
    let schedule = initSchedule sc ++ fs
    res <- mapM (runCommand now twInfo) schedule 
    saveScheduleInfo $ filter isOldSchedule res
  where 
    initSchedule :: [Schedule] -> [(Schedule, Maybe LocalTime)]
    initSchedule = map (flip (,) Nothing)

runCommand :: LocalTime -> TWInfo -> (Schedule, Maybe LocalTime) -> IO (Schedule, Maybe LocalTime)
runCommand now tw st = do 
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
    run (SchedulePlans lt str) = 
      postTweet tw $ "@its_out_of_tune " ++ show lt ++ "に、「" ++ str ++ "」って言うよ"

----
-- 認証系

murAuth :: MurmurAuth 
murAuth = MurmurAuth
  { murConsumerKey = consumerKey
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

maybeListToList :: Maybe [a] -> [a]
maybeListToList (Just xs) = xs
maybeListToList Nothing = []

getNow :: IO LocalTime
getNow = do
  utc <- getZonedTime >>= return . zonedTimeToUTC
  timeZone <- getCurrentTimeZone
  return $ utcToLocalTime timeZone utc
