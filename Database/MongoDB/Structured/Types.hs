{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-- | This module exports a 'Structued' type class which can be used to
-- convert Haskel \"record types\" to @BSON@ objects and vice versa.
-- As a Mongo document has an \"_id\" field, we impose the requirement
-- a record type have a field whose type is 'SObjId' (corresponding to
-- \"_id\").
module Database.MongoDB.Structured.Types ( -- * Structured \"_id\"
                                            SObjId(..)
                                          , noSObjId, isNoSObjId
                                          , toSObjId, unSObjId
                                          ) where
import Data.Bson
import Data.Typeable


-- | Type corresponding to the \"_id\" field of a document in a
-- structured object.
newtype SObjId = SObjId (Maybe ObjectId)
   deriving(Show, Read, Eq, Ord, Typeable, Val)

-- | The \"_id\" field is unset.
noSObjId :: SObjId
noSObjId = SObjId Nothing

-- | Check if the \"_id\" field is unset.
isNoSObjId :: SObjId -> Bool
isNoSObjId = (==) noSObjId

-- | Get the \"_id\" field (assumes that it is set0.
unSObjId :: SObjId -> ObjectId
unSObjId (SObjId (Just x)) = x
unSObjId _ = error "invalid use"

-- | Set the \"_id\" field.
toSObjId :: ObjectId -> SObjId
toSObjId = SObjId . Just
