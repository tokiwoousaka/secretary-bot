{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Web.Twitter.Murmur where
import Control.Lens
import System.IO
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Binary (sinkHandle)
import Data.Default
import Web.Authenticate.OAuth
import Web.Twitter.Conduit
import Web.Twitter.Types.Lens
import Network.HTTP.Conduit as NC
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as B8

data MurmurAuth = MurmurAuth
  { murConsumerKey :: String
  , murConsumerSecret :: String
  , murGetPINAction :: String -> IO String
  }

mur2OAuth :: MurmurAuth -> OAuth
mur2OAuth mur = twitterOAuth
  { oauthConsumerKey = B8.pack $ murConsumerKey mur
  , oauthConsumerSecret = B8.pack $ murConsumerSecret mur
  }

getTWInfo :: MurmurAuth -> IO TWInfo
getTWInfo mur = do
  mgr <- newManager tlsManagerSettings
  let oauth = mur2OAuth mur
  -- ** 権限取得処理 ** --
  accTkn <- runResourceT $ do
    -- リクエストトークン取得
    tempCred <- getTemporaryCredential oauth mgr
    let url = authorizeUrl oauth tempCred
    -- PINコードの取得
    pin <- B8.pack <$> liftIO (murGetPINAction mur url)
    -- リクエストトークン認可、アクセストークン取得
    getAccessToken oauth (insert "oauth_verifier" pin tempCred) mgr
  -- twInfoの取得
  return $ setCredential oauth accTkn def

-------- -- Post
postTweet :: TWInfo -> String -> IO ()
postTweet twInfo tweet = post twInfo (update $ T.pack tweet)

postWithReplyId :: TWInfo -> String -> Integer -> IO ()
postWithReplyId twInfo tweet id = 
  let req = (update $ T.pack tweet) & inReplyToStatusId ?~ id
  in post twInfo req

post :: TWInfo -> APIRequest n Status -> IO ()
post twInfo req = do
  mgr <- newManager tlsManagerSettings
  runResourceT $ call twInfo mgr req
  return ()

-- ShowTimeline
showMentionsTimeline :: TWInfo -> Int -> IO ()
showMentionsTimeline twInfo = showTimeline mentionsTimeline twInfo

showHomeTimeline :: TWInfo -> Int -> IO ()
showHomeTimeline twInfo = showTimeline homeTimeline twInfo

showTimeline :: HasMaxIdParam (APIRequest a [Status]) => APIRequest a [Status] -> TWInfo -> Int -> IO ()
showTimeline status twInfo ln = do
  mgr <- newManager tlsManagerSettings
  runResourceT $ do
    let src = sourceWithMaxId twInfo mgr status
    src $= CL.isolate ln $$ CL.mapM_ putTweetLn

putTweetLn :: MonadIO m => Status -> m ()
putTweetLn st = liftIO . T.putStrLn . T.concat $ 
  [ T.pack . show $ st^.statusId
  , "【"
  , st^.statusUser.userName
  , "("
  , st^.statusUser.userScreenName
  , ")】"
  , st^.statusText
  ]

-- GetTimeLine

data TweetInfo = TweetInfo 
  { twStatusId :: Integer
  , twUserName :: T.Text
  , twUserScreenName :: T.Text
  , twText :: T.Text
  } deriving (Show, Read)

getTweets :: Status -> TweetInfo
getTweets st = TweetInfo
  { twStatusId = st^.statusId
  , twUserName = st^.statusUser.userName
  , twUserScreenName = st^.statusUser.userScreenName
  , twText = st^.statusText
  } 

getMentionsTimeline :: TWInfo -> Int -> IO [TweetInfo]
getMentionsTimeline twInfo = getTimeline mentionsTimeline twInfo

getHomeTimeline :: TWInfo -> Int -> IO [TweetInfo]
getHomeTimeline twInfo = getTimeline homeTimeline twInfo

getTimeline :: HasMaxIdParam (APIRequest a [Status]) => APIRequest a [Status] -> TWInfo -> Int -> IO [TweetInfo]
getTimeline status twInfo ln = do
  mgr <- newManager tlsManagerSettings
  runResourceT $ do
    let src = sourceWithMaxId twInfo mgr status
    src $= CL.isolate ln $= CL.map getTweets $$ CL.consume


