{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | This is an arguably convenient wrapper around gogol's `Network.Google.FireStore`. This is likely not the idiomatic or best API for Google Firestore\/Datastore\/Firebase. I don't even understand the difference between all of those trademarks. Bug reports and suggestions are welcome.
module Database.Firestore
  ( -- * The types
    Document (..),
    formDocument,
    Value (..),
    FireStore,

    -- * Running `FireStore`
    runFireStore,
    defaultEnvironment,

    -- * CRUD
    listAllDocuments,
    patchDocument,
    getDocument,

    -- * Other stuff

    -- | You can check out the docs for the additional types, as well as the lenses in the `Database.Firestore.Types` module:
    module Database.Firestore.Types,
  )
where

import Control.Lens (filtered, (&), (.~), (<&>), (^.))
import Control.Monad.Trans.Resource
-- import Data.Aeson
import Data.Data
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text
import Database.Firestore.Internal
import Database.Firestore.Types
import Network.Google (AllowScopes, HasEnv)
import qualified Network.Google as Google
import qualified Network.Google.Auth.Scope as Google
import qualified Network.Google.FireStore as FireStore
import Network.HTTP.Types.Status (Status (Status, statusCode))
import System.IO (stderr)

-- | This initializes the the google environment with stderr logging, tls manager, and "application default" credentials. (@gcloud auth login@ on your local machine, or maybe @gcloud auth application-default login@, and it will also probably "just work" with the relevant service account (e.g. "compute") in the cloud). It will only have the scopes needed for FireStore (`FireStoreScope`).
--
-- This is just pure convenience.
--
-- > do
-- >   env <- defaultEnvironment
-- >   result <- runFireStore env "myproject" someFireStoreAction
defaultEnvironment :: IO (Google.Env FireStoreScope)
defaultEnvironment = do
  lgr <- Google.newLogger Google.Info stderr
  mgr <- Google.newManager Google.tlsManagerSettings
  crd <- Google.getApplicationDefault mgr
  Google.newEnvWith crd lgr mgr <&> (Google.envScopes .~ Proxy @FireStoreScope)

-- | Runs the FireStore monad. It needs the env and the project name.
--
-- > runFireStore env "myproject" someFireStoreAction
runFireStore ::
  ( MonadUnliftIO m,
    HasEnv s env,
    AllowScopes s,
    Google.HasScope' s FireStoreScope ~ 'True
  ) =>
  env ->
  Text.Text ->
  FireStore a ->
  m a
runFireStore env project (FireStore _action) = runResourceT $ Google.runGoogle env (_action project)

listDocumentsOnePage :: Text.Text -> Maybe PageToken -> FireStore ([Document], Maybe PageToken)
listDocumentsOnePage collectionName np =
  do
    res <- FireStore $
      \projectName ->
        FireStore.projectsDatabasesDocumentsList
          ("projects/" <> projectName <> "/databases/(default)/documents/" <> collectionName)
          ""
          & FireStore.pPageToken .~ fmap _unPageToken np
          & Google.send
    return (map parseDocument $ res ^. FireStore.ldrDocuments, PageToken <$> res ^. FireStore.ldrNextPageToken)

-- | It only wants the collection name itself. In the language of the Google's resource identifiers,
-- this will get into @("projects\/" <> projectName <> "\/databases\/(default)\/documents\/" <> collectionName)@.
-- Apparently, @(default)@ is the actual real name of the only database you can have.
listAllDocuments :: Text.Text -> FireStore [Document]
listAllDocuments collectionName = getPage Nothing
  where
    getPage pt =
      do
        (res, np) <- listDocumentsOnePage collectionName pt
        case np of
          Nothing -> return res
          Just pt' -> (res ++) <$> getPage (Just pt')

getDocument :: Text.Text -> Text.Text -> FireStore (Maybe Document)
getDocument collectionName name = do
  res <-
    ( FireStore $
        \projectName ->
          FireStore.projectsDatabasesDocumentsGet ("projects/" <> projectName <> "/databases/(default)/documents/" <> collectionName <> "/" <> name)
            & Google.send
            & Google.trying (Google._ServiceError . filtered (\Google.ServiceError' {Google._serviceStatus = Status {statusCode}} -> statusCode == 404))
    ) ::
      FireStore (Either Google.ServiceError FireStore.Document)

  let resM = either (const Nothing) Just res

  return $ parseDocument <$> resM

-- | This sets the fields that the passed Document contains, but won't change the already existing fields
-- that are not present in the passed Document.
patchDocument :: Text.Text -> Document -> FireStore Document
patchDocument _ (Document {name = Nothing}) = error "Can't patch a document without a name"
patchDocument collectionName document@Document {name = Just name} =
  do
    res <- FireStore $
      \projectName ->
        FireStore.projectsDatabasesDocumentsPatch
          (buildDocument document)
          ("projects/" <> projectName <> "/databases/(default)/documents/" <> collectionName <> "/" <> name)
          & FireStore.pddpUpdateMaskFieldPaths .~ (HM.keys $ fields document)
          & Google.send

    return $ parseDocument res
