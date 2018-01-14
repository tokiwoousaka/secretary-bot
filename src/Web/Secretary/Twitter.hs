{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Web.Secretary.Twitter where
import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.Binary (sinkHandle)
import Data.Default
import Network.HTTP.Conduit as NC
import System.IO
import Web.Authenticate.OAuth
import Web.Twitter.Conduit
import Web.Twitter.Types.Lens
import qualified Data.ByteString.Char8 as B8
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as T

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


