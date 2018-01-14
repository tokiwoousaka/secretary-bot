module Web.Secretary.Util where
import Data.Time
  ( getZonedTime
  , zonedTimeToUTC
  , getCurrentTimeZone
  , utcToLocalTime
  , LocalTime
  )

maybeListToList :: Maybe [a] -> [a]
maybeListToList (Just xs) = xs
maybeListToList Nothing = []

getNow :: IO LocalTime
getNow = do
  utc <- getZonedTime >>= return . zonedTimeToUTC
  timeZone <- getCurrentTimeZone
  return $ utcToLocalTime timeZone utc

