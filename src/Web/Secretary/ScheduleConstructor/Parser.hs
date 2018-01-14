{-# LANGUAGE OverloadedStrings #-}
module Web.Secretary.ScheduleConstructor.Parser where
import Control.Applicative
import Control.Monad.State
import Data.Attoparsec.Text
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
    
