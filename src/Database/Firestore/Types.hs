{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Firestore.Types where

import Control.Lens (makePrisms)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import Data.Aeson (ToJSON (..))
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics
import Network.Google (MonadGoogle)
import qualified Network.Google.Auth.Scope as Google

type FireStoreScope = '["https://www.googleapis.com/auth/cloud-platform", "https://www.googleapis.com/auth/datastore"]

data Document = Document
  { -- | Don't set it if you want FireStore to generate it for you
    name :: Maybe Text,
    createTime :: Maybe UTCTime,
    updateTime :: Maybe UTCTime,
    fields :: HM.HashMap Text Value
  }
  deriving (Show, Eq, Generic)

-- | Handy way to make a document
formDocument :: Maybe Text -> [(Text, Value)] -> Document
formDocument name vals = Document name Nothing Nothing $ HM.fromList vals

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
  --   Unknown Text
  deriving (Show, Eq, Generic)

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

-- toJSON (Unknown v) = toJSON (show v)

instance ToJSON Document

-- | This is just a convenience monad that allows one to avoid all the gogol machinery.
newtype FireStore a = FireStore
  { _action :: Text -> forall m s. (Monad m, MonadUnliftIO m, MonadGoogle s m, Google.HasScope' s FireStoreScope ~ True) => m a
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

instance MonadUnliftIO FireStore where
  withRunInIO inner = FireStore $ \name -> withRunInIO $ \run -> inner (run . ($ name) . _action)

newtype PageToken = PageToken {_unPageToken :: Text}
  deriving (Show)
