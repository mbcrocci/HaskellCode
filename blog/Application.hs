{-# LANGUAGE OverloadedStrings #-}
module Application where

import Control.Monad
import Control.Monad.IO.Class
import Blog.Common
import Data.Aeson
import qualified Data.ByteString.Char8 as S8
import Data.List
import Network.HTTP.Types
import System.Directory
import System.FilePath
import System.IO
import Web.Frank
import Web.Simple
import Web.Simple.Templates

app :: (Application -> IO ()) -> IO ()
app runner = do
  settings <- newAppSettings

  runner $ controllerApp settings $ do
    get "/" $ do
      posts <- liftIO $ do
        dataDir <- getDirectoryContents "data"
        let postFiles = sort $
              filter (not . isPrefixOf ".") dataDir
        forM postFiles $ \postFile -> do
          withFile ("data" </> postFile) ReadMode $ \h -> do
            title <- hGetLine h
            return $ object ["id" .= postFile
                            , "title" .= title]
      render "index.html" $ object ["posts" .= posts]

    -- Respond to "/new"
    get "/new" $ do
      render "new.html" ()

    -- Respond to "/:post_id"
    get "/:post_id" $ routeTop $ do
      postId <- queryParam' "post_id"
      let postFile = "data" </> (takeFileName postId)
      post <- liftIO $ do
        h <- openFile postFile ReadMode
        title <- hGetLine h
        body <- hGetContents h
        return $ object ["title" .= title, "body" .= body]
      render "show.html" post

    -- Create form
    post "/" $ do
      (params, _) <- parseForm
      let notNull = not . S8.null
      let mpost = do
            title <- notNull `mfilter` lookup "title" params
            body <- notNull `mfilter` lookup "body" params
            return (title, body)
      case mpost of
        Nothing -> redirectBack
        Just (title, body) -> liftIO $ do
          files <- filter (\(c:_) -> c /= '.') `fmap`
            getDirectoryContents "data"
          let lastFileNum = show $ length files + 1
          let fileName =
                take (5 - length lastFileNum)
                  [z | _ <- [0..], let z = '0'] ++
                lastFileNum
          withFile ("data" </> fileName) WriteMode $ \h -> do
            S8.hPutStrLn h title
            S8.hPutStr h body
      respond $ redirectTo "/"
