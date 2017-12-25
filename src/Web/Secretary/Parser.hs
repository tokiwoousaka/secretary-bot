{-# LANGUAGE OverloadedStrings #-}
module Web.Secretary.Parser where
import qualified Web.Twitter.Conduit (TWInfo)
import Control.Applicative
import Control.Monad.State
import Data.Attoparsec.Text
import Data.List.Extra (trim)
import Data.Time
import qualified Data.Text as T

data CommandToken
  = TokenSay
  | TokenAccount String
  | TokenAnyChar Char
  | TokenYear Integer
  | TokenMonth Int
  | TokenDay Int
  | TokenMonthDay Int Int
  | TokenYearMonthDay Integer Int Int
  | TokenHour Int
  | TokenMinites Int
  | TokenHourMinites Int Int
  deriving (Show, Read)

yearParser :: Parser CommandToken
yearParser = TokenYear <$> decimal <* "年"

monthParser :: Parser CommandToken
monthParser = TokenMonth <$> decimal <* "月"

monthDayParser :: Parser CommandToken
monthDayParser = TokenMonthDay <$> decimal <* "/" <*> decimal

yearMonthDayParser :: Parser CommandToken
yearMonthDayParser = TokenYearMonthDay <$> decimal <* "/" <*> decimal <* "/" <*> decimal

dayParser :: Parser CommandToken
dayParser = TokenDay <$> decimal <* "日"

hourParser :: Parser CommandToken
hourParser = TokenHour <$> decimal <* "時"

minitesParser :: Parser CommandToken
minitesParser = TokenMinites <$> decimal <* "分"

hourMinitesParser :: Parser CommandToken
hourMinitesParser = TokenHourMinites <$> decimal <* ":" <*> decimal

dateTimeParser :: Parser CommandToken
dateTimeParser 
  =   yearMonthDayParser
  <|> monthDayParser
  <|> hourMinitesParser
  <|> yearParser 
  <|> monthParser
  <|> dayParser
  <|> hourParser
  <|> minitesParser

sayParser :: Parser CommandToken
sayParser = string "って言って" *> pure TokenSay

accountParser :: Parser CommandToken
accountParser = TokenAccount <$> (char '@' *> many1 (chars "abcdefghijklmnopqrstuvwxyz_"))
  where
    chars :: String -> Parser Char
    chars [] = error "chars request some char"
    chars (c:cs) = foldl (<|>) (char c) $ map char cs

tokenParser :: Parser CommandToken
tokenParser 
  =   accountParser
  <|> dateTimeParser
  <|> sayParser 
  <|> TokenAnyChar <$> anyChar

tokensParser :: Parser [CommandToken]
tokensParser 
  =   pure [] <* endOfInput 
  <|> (:) <$> tokenParser <*> tokensParser

parseToken :: T.Text -> Maybe [CommandToken]
parseToken text = 
  case parse tokensParser text `feed` "" of
    Done _ res -> Just res
    _ -> Nothing
    
parseCommand :: LocalTime -> T.Text -> Maybe [Schedule]
parseCommand now = fmap (tokensToCommands now) . parseToken 

-----

data Schedule 
  = ScheduleLocalTime LocalTime String
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
    let (ys, command) = evalState (token2Cmd xs) ("", defaultDateTime)
    case command of
      Just cmd -> cmd : tokensToCommands now ys
      _ -> tokensToCommands now ys
  where 
    token2Cmd :: [CommandToken] -> State (String, DateTime) ([CommandToken], Maybe Schedule)
    token2Cmd [] = return ([], Nothing)
    token2Cmd (TokenSay : xs) = do
      str <- fmap fst $ get
      dt <- fmap snd $ get
      put ("", defaultDateTime)
      return (xs, Just . constructSchedule dt . reverse $ trim str)
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

