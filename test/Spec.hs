{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import Web.Secretary.ScheduleConstructor.Parser 

main :: IO ()
main = do
  runTestTT $ TestList
    [ parserTest
    ]
  return ()

parserTest :: Test
parserTest = TestList
  [ "parser test 1" ~:
      parseToken "ほげって言って" ~?= Just [TokenSay]
  ]
