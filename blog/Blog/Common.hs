{-# LANGUAGE MultiParamTypeClasses #-}
module Blog.Common where

import Control.Applicative
import Web.Simple
import Web.Simple.Templates



data AppSettings = AppSettings {  }

newAppSettings :: IO AppSettings
newAppSettings = do
  
  return $ AppSettings

instance HasTemplates IO AppSettings where
  defaultLayout = Just <$> getTemplate "layouts/main.html"

