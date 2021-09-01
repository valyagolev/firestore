{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- {-# LANGUAGE NoImplicitPrelude #-}

module Database.Firestore.Direct where

import Control.Applicative (Applicative (liftA2))
import Control.Exception (SomeException (SomeException), try)
import Control.Lens
  ( makePrisms,
    (&),
    (.~),
    (<&>),
    (?~),
    (^.),
    (^?),
    _Just,
  )
-- import Data.Aeson
--   ( FromJSON (parseJSON),
--     Options (fieldLabelModifier),
--     Result,
--     ToJSON (toEncoding, toJSON),
--     Value (..),
--     camelTo2,
--     defaultOptions,
--     fromJSON,
--     genericParseJSON,
--     genericToEncoding,
--   )
-- import Data.Aeson.Lens
-- import Data.Aeson.Types (FromJSON)

-- import Data.Aeson
--   ( FromJSON (parseJSON),
--     Options (fieldLabelModifier),
--     Result,
--     ToJSON (toEncoding, toJSON),
--     Value (..),
--     camelTo2,
--     defaultOptions,
--     fromJSON,
--     genericParseJSON,
--     genericToEncoding,
--   )
-- import Data.Aeson.Lens
-- import Data.Aeson.Types (FromJSON)

import Control.Lens.Internal.Zoom (May)
import Control.Monad (Functor, join)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Resource (MonadUnliftIO, runResourceT)
import Data.Aeson (ToJSON (toJSON), defaultOptions, genericToJSON)
import qualified Data.ByteString as BS
import Data.Data (Data (gfoldl, gmapQ))
import Data.Foldable (msum)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Scientific (toRealFloat)
import Data.Text (Text, takeWhileEnd)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.Vector (fromList)
import qualified Data.Vector as V
import Debug.Trace (trace, traceShow, traceShowId)
import GHC.Generics
import Network.Google
import qualified Network.Google as FireStore
import qualified Network.Google as Google
import qualified Network.Google.Auth.Scope as Google
import Network.Google.FireStore (dCreateTime, dUpdateTime)
import qualified Network.Google.FireStore as FireStore
import qualified Network.Google.FireStore.Types as FireStore
import Network.HTTP.Types.Status
import System.IO (stderr)

-- import Prelude (Bool (..), Double, IO, Monad, map, pure, (<*>), (>>=))

type FireStoreScope = '["https://www.googleapis.com/auth/cloud-platform", "https://www.googleapis.com/auth/datastore"]

defaultEnvironment :: IO (Google.Env FireStoreScope)
defaultEnvironment = do
  lgr <- Google.newLogger Google.Trace stderr
  mgr <- Google.newManager Google.tlsManagerSettings
  crd <- Google.getApplicationDefault mgr
  Google.newEnvWith crd lgr mgr <&> (Google.envScopes .~ Proxy @FireStoreScope)

newtype FireStore a = FireStore
  { _action :: Text -> forall m s. (Monad m, MonadGoogle s m, Google.HasScope' s FireStoreScope ~ True) => m a
  }
  deriving (Functor)

instance Applicative FireStore where
  pure a = FireStore $ const $ return a
  FireStore f <*> FireStore a = FireStore $ \projectName -> f projectName <*> a projectName

instance Monad FireStore where
  FireStore f >>= g = FireStore $ \projectName ->
    do
      f' <- f projectName
      _action (g f') projectName

instance MonadIO FireStore where
  liftIO a = FireStore $ const (liftIO a)

runFireStore ::
  ( MonadUnliftIO m,
    HasEnv s env,
    AllowScopes s,
    Google.HasScope' s FireStoreScope ~ 'True
  ) =>
  env ->
  Text ->
  FireStore a ->
  m a
runFireStore env project (FireStore _action) = runResourceT $ Google.runGoogle env (_action project)

newtype PageToken = PageToken {_unPageToken :: Text}
  deriving (Show)

data Document = Document
  { name :: Maybe Text,
    createTime :: Maybe UTCTime,
    updateTime :: Maybe UTCTime,
    fields :: HM.HashMap Text Value
  }
  deriving (Show, Generic, Eq)

data Value
  = GeoPoint Double Double
  | Bytes BS.ByteString
  | Int Integer
  | Timestamp UTCTime
  | Double Double
  | String Text
  | Bool Bool
  | Map (HM.HashMap Text Value)
  | Array [Value]
  | Reference Text
  | Null
  | Unknown Text
  deriving (Generic, Eq, Data, Show)

makePrisms ''Value

instance ToJSON Value where
  toJSON (GeoPoint lat long) = toJSON [lat, long]
  toJSON (Bytes bs) = toJSON (show bs)
  toJSON (Int i) = toJSON i
  toJSON (Timestamp t) = toJSON t
  toJSON (Double d) = toJSON d
  toJSON (String t) = toJSON t
  toJSON (Bool b) = toJSON b
  toJSON (Map m) = toJSON m
  toJSON (Array xs) = toJSON xs
  toJSON (Reference t) = toJSON t
  toJSON Null = toJSON (Nothing :: Maybe ())
  toJSON (Unknown v) = toJSON (show v)

instance ToJSON Document

parseValue :: FireStore.Value -> Value
parseValue v =
  fromMaybe
    -- (Unknown $ Text.pack $ show v)
    Null -- (it's a bug in gogol-firestore currently, apparently... https://github.com/brendanhay/gogol/issues/170)
    $ msum
      [ liftA2 GeoPoint (join $ v ^? (FireStore.vGeoPointValue . _Just . FireStore.llLatitude)) (join $ v ^? (FireStore.vGeoPointValue . _Just . FireStore.llLongitude)),
        Bytes <$> v ^. FireStore.vBytesValue,
        Int . fromIntegral <$> v ^. FireStore.vIntegerValue,
        Timestamp <$> v ^. FireStore.vTimestampValue,
        Double <$> v ^. FireStore.vDoubleValue,
        String <$> v ^. FireStore.vStringValue,
        Bool <$> v ^. FireStore.vBooleanValue,
        Map . fmap parseValue
          <$> v ^? (FireStore.vMapValue . _Just . FireStore.mvFields . _Just . FireStore.mvfAddtional),
        Array . map parseValue <$> v ^? (FireStore.vArrayValue . _Just . FireStore.avValues),
        Reference <$> v ^. FireStore.vReferenceValue,
        Null <$ v ^. FireStore.vNullValue
      ]

buildValue :: Value -> FireStore.Value
buildValue v =
  FireStore.value
    & FireStore.vGeoPointValue
      .~ ( ( \(lat, lng) ->
               FireStore.latLng
                 & FireStore.llLatitude ?~ lat
                 & FireStore.llLongitude ?~ lng
           )
             <$> v
             ^? _GeoPoint
         )
    & FireStore.vBytesValue .~ (v ^? _Bytes)
    & FireStore.vIntegerValue .~ (fromIntegral <$> v ^? _Int)
    & FireStore.vTimestampValue .~ (v ^? _Timestamp)
    & FireStore.vDoubleValue .~ (v ^? _Double)
    & FireStore.vStringValue .~ (v ^? _String)
    & FireStore.vBooleanValue .~ (v ^? _Bool)
    & FireStore.vMapValue
      .~ ( (\v -> FireStore.mapValue & FireStore.mvFields ?~ FireStore.mapValueFields v) . fmap buildValue <$> v ^? _Map
         )
    & FireStore.vArrayValue .~ ((\v -> FireStore.arrayValue & FireStore.avValues .~ v) . map buildValue <$> v ^? _Array)
    & FireStore.vReferenceValue .~ (v ^? _Reference)
    & FireStore.vNullValue .~ (FireStore.NullValue <$ v ^? _Null)

parseDocument :: FireStore.Document -> Document
parseDocument d =
  Document
    (d ^. FireStore.dName)
    (d ^. FireStore.dCreateTime)
    (d ^. FireStore.dUpdateTime)
    (parseValue <$> d ^. FireStore.dFields . _Just . FireStore.dfAddtional)

buildDocument :: Document -> FireStore.Document
buildDocument (Document {name, fields}) =
  FireStore.document
    & FireStore.dName .~ name
    & FireStore.dFields
      ?~ ( FireStore.documentFields undefined -- (buildValue fields)
         )

listDocumentsOnePage :: Text -> Maybe PageToken -> FireStore ([Document], Maybe PageToken)
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

listAllDocuments :: Text -> FireStore [Document]
listAllDocuments collectionName = getPage Nothing
  where
    getPage pt =
      do
        (res, np) <- listDocumentsOnePage collectionName pt
        case np of
          Nothing -> return res
          Just pt' -> (res ++) <$> getPage (Just pt')

patchDocument :: Text -> Document -> FireStore Document
patchDocument path document =
  do
    res <- FireStore $
      \projectName ->
        FireStore.projectsDatabasesDocumentsPatch
          (buildDocument document)
          ("projects/" <> projectName <> "/databases/(default)/documents/" <> path)
          & Google.send

    return $ parseDocument res