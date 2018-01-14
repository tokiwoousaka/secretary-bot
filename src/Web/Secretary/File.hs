module Web.Secretary.File where
import Web.Twitter.Conduit (TWInfo)
import Web.Secretary.ScheduleConstructor
import System.Directory
import Data.Time
import Text.Read

loadScheduleInfo :: IO [(Schedule, Maybe LocalTime)]
loadScheduleInfo = scheduleInfoFileName >>= loadFileInfo []

loadTlInfo :: IO Integer
loadTlInfo = tlInfoFileName >>= loadFileInfo 0

saveScheduleInfo :: [(Schedule, Maybe LocalTime)] -> IO ()
saveScheduleInfo v = scheduleInfoFileName >>= saveFileInfo v

saveTlInfo :: Integer -> IO ()
saveTlInfo v = tlInfoFileName >>= saveFileInfo v

-----

readTWInfo :: IO TWInfo
readTWInfo = twInfoFileName >>= readFile >>= return . read

writeTWInfo :: TWInfo -> IO ()
writeTWInfo twInfo = twInfoFileName >>= flip writeFile (show twInfo)

-----

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


saveFileInfo :: Show a => a -> String -> IO ()
saveFileInfo v fn = writeFile fn $ show v

-----

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

