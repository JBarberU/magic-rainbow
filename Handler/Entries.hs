module Handler.Entries where

import Yesod.Auth
import Import
import Data.Time (getCurrentTime)
import Yesod.Form.Nic (nicHtmlField)

getEntryR :: EntryId -> Handler Html
getEntryR entryid = do
  entry <- runDB $ get404 entryid
  muser <- maybeAuth
  comments <- runDB $ selectList [CommentEntry ==. entryid] [Desc CommentCreated]
  (commentFormWidget, enctype) <- generateFormPost $ writeCommentForm entryid
  defaultLayout $(widgetFile "entry")
  

postEntryR :: EntryId -> Handler ()
postEntryR entryid = do
    ((result, _), _) <- runFormPost $ writeCommentForm entryid
    case result of
      FormSuccess comment -> do
        _ <- runDB $ insert comment
        setMessage "Comment created"
        redirect $ EntryR entryid
      _ -> do
        setMessageI MsgInvalidInput
        redirect $ EntryR entryid


getEntriesR :: Handler Html
getEntriesR = do
  entries <- runDB $ selectList [] [Desc EntryCreated]
  defaultLayout $(widgetFile "entries")

getWriteEntryR :: Handler Html
getWriteEntryR = do
    uid <- requireAuthId
    (formWidget, enctype) <- generateFormPost writeEntryForm
    defaultLayout $(widgetFile "writeentry")

postWriteEntryR :: Handler ()
postWriteEntryR = do
    ((result, _), _) <- runFormPost writeEntryForm
    case result of
      FormSuccess entry -> do
        _ <- runDB $ insert entry
        setMessageI MsgEntryCreated
        redirect $ EntriesR
      _ -> do
        setMessageI MsgInvalidInput
        redirect $ WriteEntryR 

writeEntryForm = renderBootstrap $ Entry
  <$> areq textField (fieldSettingsLabel MsgTitle) Nothing
  <*> areq nicHtmlField (fieldSettingsLabel MsgContent) Nothing
  <*> lift (liftIO getCurrentTime)

writeCommentForm entryid = renderBootstrap $ Comment 
  <$> pure entryid
  <*> areq textareaField "Comment" Nothing
  <*> lift (liftIO getCurrentTime)


