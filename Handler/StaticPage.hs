module Handler.StaticPage where

import Import
import Data.Time (getCurrentTime)
import Yesod.Form.Nic (nicHtmlField)
import Yesod.Auth (requireAuthId)

getStaticPagesR :: Handler Html
getStaticPagesR = do
  _ <- requireAuthId
  (formWidget, enctype) <- generateFormPost staticPageForm
  sPages <- runDB $ selectList [] [Desc StaticPageCreated]  
  defaultLayout $(widgetFile "staticpages")

staticPageForm = renderBootstrap $ StaticPage
  <$> areq textField (fieldSettingsLabel MsgTitle) Nothing
  <*> areq nicHtmlField (fieldSettingsLabel MsgContent) Nothing
  <*> lift (liftIO getCurrentTime)

postStaticPagesR :: Handler Html
postStaticPagesR = do 
    ((result, _), _) <- runFormPost staticPageForm
    case result of
      FormSuccess staticPage -> do
        _ <- runDB $ insert staticPage
        setMessageI MsgStaticPageCreated
        redirect $ StaticPagesR
      _ -> do
        setMessageI MsgInvalidInput
        redirect $ StaticPagesR


getStaticPageR :: StaticPageId -> Handler Html
getStaticPageR pageid = do
  sPage <- runDB $ get404 pageid
  defaultLayout $(widgetFile "staticpage")

