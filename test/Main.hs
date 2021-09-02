{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Internal.Strict as HM
import qualified Data.HashMap.Strict as HS
import Data.Text (Text)
import Database.Firestore
import Database.Firestore.Internal
import Debug.Trace (trace, traceId, traceShowId)
import GHC.Generics (Generic)
import Generic.Random
import qualified Network.Google.FireStore.Types as FireStore
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary (Arbitrary)
import Test.QuickCheck.Instances.ByteString
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.Time
import Test.QuickCheck.Instances.UnorderedContainers

-- import

realTest :: IO ()
realTest = do
  env <- defaultEnvironment
  runFireStore env "carboncopy" $ do
    docs <- listAllDocuments "rss_subscriptions"
    -- (docs, _) <- listDocumentsOnePage "rss_subscriptions" Nothing -- :: IO (Result ([BotUser], Maybe NextPage))
    liftIO $ do
      print "---"
      BS.putStrLn $ encodePretty $ head docs
      print "---"
      print $ length docs
      BS.writeFile "test.json" $ encodePretty $ docs

-- instance Arbitrary (HS.HashMap Text Value) where
--   arbitrary = HS.fromList <$> arbitrary
--   shrink m = HS.fromList <$> shrink (HS.toList m)

-- instance Arbitrary FireStore.Value where
--   arbitrary = genericArbitraryRec uniform
--   shrink = genericShrink

instance Arbitrary Value where
  -- fixme
  arbitrary = sized $ genericArbitraryRec uniform `withBaseCase` (String <$> arbitrary)
  shrink = genericShrink

prop_roundtrip_value v = parseValue (buildValue $ traceShowId v) == v

prop_roundtrip_value' v = buildValue (parseValue v) == v

main :: IO ()
main =
  do
    -- print "1"
    -- print $ parseValue $ FireStore.value
    -- print "2"

    -- print $ Array [Map (HM.fromList [("", GeoPoint 0.0 0.0)])]
    -- print $ parseValue . buildValue $ Array [Map (HM.fromList [("", GeoPoint 0.0 0.0)])]
    quickCheck $ prop_roundtrip_value
    realTest

-- quickCheck $ prop_roundtrip_value'