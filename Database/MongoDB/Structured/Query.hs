{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-| This module exports several classes and combinators that operated on
  'Structured' types. Specifically, we provide the structured versions
  of @mongoDB@''s combinators, including structured query creation.
-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.MongoDB.Structured.Query (
                                          -- * Insert
                                           insert, insert_
                                         , insertMany, insertMany_
                                         , insertAll, insertAll_
                                         -- * Update
                                         , save
                                         -- * Delete
                                         , delete, deleteOne
                                         -- * Order
                                         , asc
                                         , desc
                                         -- * Query
                                         , StructuredQuery
                                         , limit
                                         , skip
                                         , sort
                                         , find
                                         , findOne
                                         , fetch
                                         , count
                                         -- * Structured selections/queries
                                         , StructuredSelection
                                         , StructuredSelect(select)
                                         , Selectable(..)
                                         , (.!)
                                         , QueryExp(..)
                                         , (.*)
                                         , (.==), (./=), (.<), (.<=), (.>), (.>=)
                                         , (.&&), (.||), not_
                                         -- * Cursor
                                         , StructuredCursor
                                         , closeCursor, isCursorClosed
                                         , next, nextBatch, nextBatch'
                                         , nextN, nextN'
                                         , rest, rest'
                                         -- * Rexports
                                         , module Database.MongoDB.Query
                                         , Value
                                         ) where

import qualified Database.MongoDB.Query as M
import Database.MongoDB.Query (Action
                              , access
                              , Failure(..)
                              , ErrorCode
                              , AccessMode(..)
                              , GetLastError
                              , master
                              , slaveOk
                              , accessMode
                              , Database
                              , allDatabases
                              , useDb
                              , thisDatabase
                              , Username
                              , Password
                              , auth)
import Data.Bson
import qualified Data.Text as T
import Data.Maybe (fromJust, catMaybes, mapMaybe)
import Data.List (sortBy, groupBy)
import Data.Functor
import Data.Typeable
import Data.Word
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Base


--
-- Insert
--

valueToObjId :: Value -> Maybe ObjectId
valueToObjId (ObjId oid) = Just oid
valueToObjId _           = Nothing

collection :: Typeable a => a -> T.Text
collection x = T.pack $ tyConModule tr ++ tyConName tr
  where
    tr = typeRepTyCon $ typeOf x

val' :: Val a => a -> [Field]
val' = undefined

cast'' :: Val a => [Field] -> Maybe a
cast'' = undefined

-- | Inserts document to its corresponding collection and return
-- the \"_id\" value.
insert :: (MonadIO m, Typeable a, Val a) => a -> Action m (Maybe ObjectId)
insert x = liftM valueToObjId $ M.insert (collection x) (val' x)

-- | Same as 'insert' but discarding result.
insert_ :: (MonadIO m, Typeable a, Val a) => a -> Action m ()
insert_ x = insert x >> return ()

-- | Inserts documents to their corresponding collection and return
-- their \"_id\" values.
insertMany :: (MonadIO m, Typeable a, Val a) => [a] -> Action m [ObjectId]
insertMany = insertManyOrAll (M.insertMany)

-- | Same as 'insertMany' but discarding result.
insertMany_ :: (MonadIO m, Typeable a, Val a) => [a] -> Action m ()
insertMany_ ss = insertMany ss >> return ()

-- | Inserts documents to their corresponding collection and return
-- their \"_id\" values. Unlike 'insertMany', this function keeps
-- inserting remaining documents even if an error occurs.
insertAll :: (MonadIO m, Typeable a, Val a) => [a] -> Action m [ObjectId]
insertAll = insertManyOrAll (M.insertAll)

-- | Same as 'insertAll' but discarding result.
insertAll_ :: (MonadIO m, Typeable a, Val a) => [a] -> Action m ()
insertAll_ ss = insertAll ss >> return ()

-- | Helper function that carries out the hard work for 'insertMany'
-- and 'insertAll'.
insertManyOrAll :: (MonadIO m, Typeable a, Val a) =>
   (M.Collection -> [Document] -> Action m [Value]) -> [a] -> Action m [ObjectId]
insertManyOrAll insertFunc ss = do
  let docs  = map (\x -> (collection x, val' x)) ss
      gdocs = (groupBy (\(a,_) (b,_) -> a == b))
              . (sortBy (\(a,_) (b,_) -> compare a b)) $ docs
  rdocs <- (forM gdocs $ \ds ->
                if (null ds)
                  then return []
                  else insertFunc (fst . head $ ds) (map snd ds)
             )
  return $ mapMaybe valueToObjId $ concat rdocs

--
-- Update
--

-- | Save document to collection. If the 'SObjId' field is set then
-- the document is updated, otherwise we perform an insert.
save :: (MonadIO m, Typeable a, Val a) => a -> Action m ()
save x = M.save (collection x) (val' x)


--
-- Delete
--

-- | Delete all documents that match the selection/query.
delete :: MonadIO m => StructuredSelection a -> Action m ()
delete = M.delete . unStructuredSelection 

-- | Delete the first documents that match the selection/query.
deleteOne :: MonadIO m => StructuredSelection a -> Action m ()
deleteOne = M.deleteOne . unStructuredSelection 


--
-- Query
--

-- | Find documents satisfying query
find :: (Functor m, MonadIO m, MonadBaseControl IO m)
     => StructuredQuery a -> Action m (StructuredCursor a)
find q = StructuredCursor <$> (M.find . unStructuredQuery $ q)

-- | Find documents satisfying query
findOne :: (MonadIO m, Typeable a, Val a)
     => StructuredQuery a -> Action m (Maybe a)
findOne q = do 
  res <- M.findOne . unStructuredQuery $ q
  return $ res >>= cast''

-- | Same as 'findOne' but throws 'DocNotFound' if none match. Error
-- is thrown if the document cannot e transformed.
fetch :: (MonadIO m, Functor m, Typeable a, Val a)
     => StructuredQuery a -> Action m a
fetch q = (fromJust . cast'') <$> (M.fetch . unStructuredQuery $ q)

-- | Count number of documents satisfying query.
count :: (MonadIO m) => StructuredQuery a -> Action m Int
count = M.count . unStructuredQuery


--
--
--

-- | Wrapper for @mongoDB@'s @Cursor@.
newtype StructuredCursor a = StructuredCursor { unStructuredCursor :: M.Cursor }

-- | Return next batch of structured documents.
nextBatch :: (Typeable a, Val a, Functor m, MonadIO m, MonadBaseControl IO m)
          => StructuredCursor a -> Action m [Maybe a]
nextBatch c = (map cast'') <$> M.nextBatch (unStructuredCursor c)

-- | Return next batch of structured documents.
nextBatch' :: (Typeable a, Val a, Functor m, MonadIO m, MonadBaseControl IO m)
          => StructuredCursor a -> Action m [a]
nextBatch' c = catMaybes <$> (map cast'') <$> M.nextBatch (unStructuredCursor c)

-- | Return next structured document. If failed return 'Left',
-- otherwise 'Right' of the deserialized result.
next :: (Typeable a, Val a, MonadIO m, MonadBaseControl IO m)
     => StructuredCursor a -> Action m (Either () (Maybe a))
next c = do
    res <- M.next (unStructuredCursor c)
    case res of
      Nothing -> return (Left ())
      Just r  -> return (Right $ cast'' r)

-- | Return up to next @N@ documents.
nextN :: (Typeable a, Val a, Functor m, MonadIO m, MonadBaseControl IO m)
      => Int -> StructuredCursor a -> Action m [Maybe a]
nextN n c = (map cast'') <$> M.nextN n (unStructuredCursor c)

-- | Return up to next @N@ valid documents.
nextN' :: (Typeable a, Val a, Functor m, MonadIO m, MonadBaseControl IO m)
      => Int -> StructuredCursor a -> Action m [a]
nextN' n c = catMaybes <$> (map cast'') <$> M.nextN n (unStructuredCursor c)


-- | Return the remaining documents in query result.
rest :: (Typeable a, Val a, Functor m, MonadIO m, MonadBaseControl IO m) 
     => StructuredCursor a -> Action m [Maybe a]
rest c = (map cast'') <$> M.rest (unStructuredCursor c)

-- | Return the remaining valid documents in query result.
rest' :: (Typeable a, Val a, Functor m, MonadIO m, MonadBaseControl IO m) 
     => StructuredCursor a -> Action m [a]
rest' c = catMaybes <$> (map cast'') <$> M.rest (unStructuredCursor c)

-- | Close the cursor.
closeCursor :: (MonadIO m, MonadBaseControl IO m) => StructuredCursor a -> Action m ()
closeCursor = M.closeCursor . unStructuredCursor

-- | Check if the cursor is closed.
isCursorClosed :: (MonadIO m, MonadBase IO m) => StructuredCursor a -> Action m Bool
isCursorClosed = M.isCursorClosed . unStructuredCursor



--
-- Structured selections/queries
--

-- | Wrapper for @mongoDB@'s @Selection@ type.
newtype StructuredSelection a =
  StructuredSelection { unStructuredSelection :: M.Selection }
  deriving(Eq, Show)

-- | Wrapper for @mongoDB@'s @Query@ type.
data StructuredQuery a = StructuredQuery
                        { selection :: StructuredSelection a
                        -- ^ Actual query.
                        , skip      :: Word32 
                        -- ^ Number of matching objects to skip
                        -- (default: 0).
                        , limit     :: Word32
                        -- ^ Maximum number of objects to return
                        -- (default: 0, no limit).
                        , sort      :: [OrderExp]
                        -- ^ Sortresult by this order.
                        }
  deriving(Eq, Show)


-- | Analog to @mongoDB@'s @Select@ class
class StructuredSelect aQorS where
  -- | Create a selection or query from an expression
  select :: (Typeable a, Val a) => QueryExp a -> aQorS a

instance StructuredSelect StructuredSelection where
  select = StructuredSelection . expToSelection

instance StructuredSelect StructuredQuery where
  select x = StructuredQuery (StructuredSelection $ expToSelection x)
                              0 0 ([])

unStructuredQuery :: StructuredQuery a -> M.Query
unStructuredQuery sq = M.Query [] -- options
                               (unStructuredSelection $ selection sq)
                               [] -- project
                               (skip sq) -- skip
                               (limit sq) -- limit
                               (expToOrder $ sort sq) -- sort
                               False 0 []

-- | Class defining a selectable type. Type 'a' corresponds to the
-- record type, 'f' corresponds to the field or facet, and 't'
-- corresponds to the field/facet type.
class Val t => Selectable a f t | f -> a, f -> t where
  -- | Given facet, return the BSON field name
  s :: f -> t -> Label

-- | Nested fields (used for extracting the names of fields in a
-- nested record). 
data Nested f f' = Nested Label

-- | Combining two field names to create a 'Nested' type.
(.!) :: (Selectable r f t, Selectable t f' t') => f -> f' -> Nested f f'
(.!) f f' = Nested $ T.intercalate (T.pack ".") [(s f undefined), (s f' undefined)]

instance (Selectable r f t, Selectable t f' t') =>
          Selectable r (Nested f f') t' where
  s (Nested l) _ = l

-- | A query expression.
data QueryExp a = StarExp
                | EqExp   !Label   !Value
                | LBinExp !Label !Label !Value
                | AndExp  (QueryExp a) (QueryExp a) 
                | OrExp   (QueryExp a) (QueryExp a) 
                | NotExp  (QueryExp a)
                deriving (Eq, Show)

infix   9 .! 
infix   4 .==, ./=, .<, .<=, .>, .>=
infixr  3 .&&
infixr  2 .||

-- | Combinator for @==@
(.*) :: (Typeable a, Val a) => QueryExp a
(.*) = StarExp

-- | Combinator for @==@
(.==) :: (Val t, Selectable a f t) => f -> t -> QueryExp a
(.==) f v = EqExp  (s f v) (val v)

-- | Combinator for @$ne@
(./=) :: (Val t, Selectable a f t) => f -> t -> QueryExp a
(./=) f v = LBinExp (T.pack "$ne") (s f v) (val v)

-- | Combinator for @<@
(.< ) :: (Val t, Selectable a f t) => f -> t -> QueryExp a
(.< ) f v = LBinExp (T.pack "$lt") (s f v) (val v)

-- | Combinator for @<=@
(.<=) :: (Val t, Selectable a f t) => f -> t -> QueryExp a
(.<=) f v = LBinExp (T.pack "$lte") (s f v) (val v)

-- | Combinator for @>@
(.> ) :: (Val t, Selectable a f t) => f -> t -> QueryExp a
(.> ) f v = LBinExp (T.pack "$gt") (s f v) (val v)

-- | Combinator for @>=@
(.>=) :: (Val t, Selectable a f t) => f -> t -> QueryExp a
(.>=) f v = LBinExp (T.pack "$gte") (s f v) (val v)

-- | Combinator for @$and@
(.&&) :: QueryExp a -> QueryExp a -> QueryExp a
(.&&) = AndExp

-- | Combinator for @$or@
(.||) :: QueryExp a -> QueryExp a -> QueryExp a
(.||) = OrExp

-- | Combinator for @$not@
not_ :: QueryExp a -> QueryExp a
not_ = NotExp

-- | Convert a query expression to a 'Selector'.
expToSelector :: (Typeable a, Val a) => QueryExp a -> M.Selector
expToSelector (StarExp)        = [ ]
expToSelector (EqExp l v)      = [ l := v ]
expToSelector (LBinExp op l v) = [ l =: [ op := v ]]
expToSelector (AndExp e1 e2)   = [ (T.pack "$and") =: [expToSelector e1
                                                 , expToSelector e2] ]
expToSelector (OrExp e1 e2)    = [ (T.pack "$or") =: [expToSelector e1
                                                , expToSelector e2] ]
expToSelector (NotExp e)       = [ (T.pack "$not") =: expToSelector e]

-- | Convert query expression to 'Selection'.
expToSelection :: (Typeable a, Val a) => QueryExp a -> M.Selection
expToSelection e = M.Select { M.selector = (expToSelector e)
                            , M.coll = (collection . c $ e) }
  where c :: (Typeable a, Val a) => QueryExp a -> a
        c _ = undefined

-- | An ordering expression
data OrderExp = Desc Label
              | Asc Label
  deriving(Eq, Show)

-- | Sort by field, ascending
asc :: Selectable a f t => f -> OrderExp
asc f = Asc (s f undefined)

-- | Sort by field, descending
desc :: Selectable a f t => f -> OrderExp
desc f = Desc (s f undefined)

-- | Convert a list of @OrderExp@ to a @mongoDB@ @Order@
expToOrder :: [OrderExp] -> M.Order
expToOrder exps = map _expToLabel exps
  where _expToLabel (Desc fieldName) = fieldName := val (-1 :: Int)
        _expToLabel (Asc fieldName) = fieldName := val (1 :: Int)

