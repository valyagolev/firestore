module Database.Firestore.Internal where

import Control.Applicative (Applicative (liftA2))
import Control.Lens
import Control.Monad (join, msum)
import Data.Maybe (fromMaybe)
import Database.Firestore.Types
import qualified Network.Google.FireStore as FireStore

-- | This module is for the interaction with `gogol` stuff.

-- | There's a bug in gogol-firestore currently, apparently... that makes "nulls" slightly ambiguous. https://github.com/brendanhay/gogol/issues/170)
parseValue :: FireStore.Value -> Value
parseValue v =
  fromMaybe
    -- (Unknown $ Text.pack $ show v)
    Null
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
      ?~ ( FireStore.documentFields (fmap buildValue fields)
         )
