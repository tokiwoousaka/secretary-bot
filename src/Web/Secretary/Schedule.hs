module Web.Secretary.Schedule where
import Web.Twitter.Conduit (TWInfo)
import Web.Secretary.ScheduleConstructor
import Web.Secretary.Twitter
import Web.Secretary.Util
import Web.Secretary.File 
  ( loadTlInfo 
  , loadScheduleInfo
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
