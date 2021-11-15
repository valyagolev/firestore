module Database.Firestore.Class where

import Database.Firestore.Types

class AsFirestore a where
  toDocument :: a -> Document
  fromDocument :: Document -> Maybe a
