{-# LANGUAGE OverloadedStrings #-}
module Web.Secretary.ScheduleConstructor where
import Control.Monad.State
import Data.List.Extra (trim)
import Data.Maybe (catMaybes)
import Data.Time 
  ( LocalTime(..)
  , fromGregorianValid
  , makeTimeOfDayValid
  , localDay
  , localTimeOfDay
  , toGregorian
  , addDays
  )
import Web.Secretary.Parser 
import qualified Data.Text as T
import qualified Web.Twitter.Conduit (TWInfo)

parseCommand :: LocalTime -> T.Text -> Maybe [Schedule]
parseCommand now = fmap (tokensToCommands now) . parseToken 

data Schedule 
  = ScheduleLocalTime LocalTime String
  | SchedulePlans LocalTime String
  | ScheduleNow String
  deriving (Show, Read)

data DateTime = DateTime
  { dtYear :: Maybe Integer
  , dtMonth :: Maybe Int
  , dtDay :: Maybe Int
  , dtHour :: Maybe Int
  , dtMinutes :: Maybe Int
  } deriving (Show, Read)

defaultDateTime :: DateTime
defaultDateTime = DateTime
  { dtYear = Nothing
  , dtMonth = Nothing
  , dtDay = Nothing
  , dtHour = Nothing
  , dtMinutes = Nothing
  } 

tokensToCommands :: LocalTime -> [CommandToken] -> [Schedule]
tokensToCommands now [] = []
tokensToCommands now xs = do
    let (ys, commands) = evalState (token2Cmd xs) ("", defaultDateTime)
    catMaybes commands ++ tokensToCommands now ys
  where 
    token2Cmd :: [CommandToken] -> State (String, DateTime) ([CommandToken], [Maybe Schedule])
    token2Cmd [] = return ([], [])
    token2Cmd (TokenSay : xs) = do
      str <- fmap fst $ get
      dt <- fmap snd $ get
      put ("", defaultDateTime)
      return (xs, map Just . addNotify . constructSchedule dt . reverse $ trim str)
    token2Cmd (TokenAnyChar c: xs) = do
      buildChar c
      token2Cmd xs
    -- date tokens
    token2Cmd (TokenYearMonthDay y m d:xs) = do
      dt <- fmap snd $ get
      putDt $ dt { dtYear = Just y, dtMonth = Just m, dtDay = Just d }
      token2Cmd xs
    token2Cmd (TokenMonthDay m d:xs) = do
      dt <- fmap snd $ get
      putDt $ dt { dtMonth = Just m, dtDay = Just d }
      token2Cmd xs
    token2Cmd (TokenYear i:xs) = do
      dt <- fmap snd $ get
      putDt $ dt { dtYear = Just i }
      token2Cmd xs
    token2Cmd (TokenMonth i:xs) = do
      dt <- fmap snd $ get
      putDt $ dt { dtMonth = Just i }
      token2Cmd xs
    token2Cmd (TokenDay i:xs) = do
      dt <- fmap snd $ get
      putDt $ dt { dtDay = Just i }
      token2Cmd xs
    token2Cmd (TokenHourMinites h m:xs) = do
      dt <- fmap snd $ get
      putDt $ dt { dtHour = Just h, dtMinutes = Just m }
      token2Cmd xs
    token2Cmd (TokenHour i:xs) = do
      dt <- fmap snd $ get
      putDt $ dt { dtHour = Just i }
      token2Cmd xs
    token2Cmd (TokenMinites i:xs) = do
      dt <- fmap snd $ get
      putDt $ dt { dtMinutes = Just i }
      token2Cmd xs
    -- unseported pattern
    token2Cmd (_:xs) = token2Cmd xs

    buildChar :: Char -> State (String, DateTime) ()
    buildChar c = modify (\(s, d) -> (c : s, d))

    putDt :: DateTime -> State (String, DateTime) ()
    putDt dt = modify (\(s, _) -> (s, dt))

    constructSchedule :: DateTime -> String -> Schedule
    constructSchedule dt = case getScheduleTime now dt of
      Just lt -> ScheduleLocalTime lt
      Nothing -> ScheduleNow

    addNotify :: Schedule -> [Schedule]
    addNotify s@(ScheduleLocalTime t m) = [s, SchedulePlans t m]
    addNotify s = [s]

getScheduleTime :: LocalTime -> DateTime -> Maybe LocalTime
getScheduleTime now dt = 
  foldl mplus Nothing [constructLocalTime dt, correctYear now dt, correctDay now dt]

constructLocalTime :: DateTime -> Maybe LocalTime
constructLocalTime dt = do
    year <- dtYear dt
    month <- dtMonth dt
    day <- dtDay dt
    hour <- dtHour dt
    minites <- dtMinutes dt
    -- construct
    lday <- fromGregorianValid year month day
    ltime <- makeTimeOfDayValid hour minites 0
    return $ LocalTime
      { localDay = lday
      , localTimeOfDay = ltime
      } 

correctYear :: LocalTime -> DateTime -> Maybe LocalTime
correctYear now dt = do
  let (year, _, _) = toGregorian . localDay $ now
  currentYear <- constructLocalTime $ dt { dtYear = Just year }
  if now < currentYear 
    then Just currentYear
    else constructLocalTime $ dt { dtYear = Just $ year + 1 }

correctDay :: LocalTime -> DateTime -> Maybe LocalTime
correctDay now dt = do
    let (year, month, day) = toGregorian . localDay $ now
    currentDay <- constructLocalTime 
      $ dt { dtYear = Just year, dtMonth = Just month, dtDay = Just day }
    if now < currentDay
      then Just currentDay
      else Just $ addDaysForLocalTime 1 currentDay
  where
    addDaysForLocalTime :: Integer -> LocalTime -> LocalTime
    addDaysForLocalTime i (LocalTime day timeOfDay) = LocalTime (addDays 1 day) timeOfDay

