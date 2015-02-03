-- To allow for structured and well-typed queies, this module generates
-- types corresponding to each field (which are made an instance of
-- 'Selectable'). Specifically, for the above data type, it creates:
-- 
-- >  data UserId = UserId deriving (Show, Eq)
-- >  instance Selectable User UserId SObjId where s _ _ = "_id"
-- >  
-- >  data FirstName = FirstName deriving (Show, Eq)
-- >  instance Selectable User FirstName String where s _ _ = "firstName"
-- >  
-- >  data LastName = LastName deriving (Show, Eq)
-- >  instance Selectable User LastName String where s _ _ = "lastName"
--
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyDataDecls #-}
module Database.MongoDB.Structured.Deriving.TH ( deriveSelectable ) where

import Database.MongoDB.Structured.Query
import Database.MongoDB.Structured
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Char (toUpper)
import qualified Data.Text as T
import Data.Functor ((<$>))

data T1
data T2
data T3

-- | Given name of type, and fields, generate new type corrsponding to
-- each field and make them instances of @Selectable@.
-- Suppose we have
--
-- >  data User = User { userId        :: SObjId
-- >                   , userFirstName :: String
-- >                   , userLastName  :: String
-- >                   }
--
-- This fucntion generates the following types and instances:
--
-- >  data UserId = UserId deriving (Show, Eq)
-- >  instance Selectable User UserId SObjId where s _ _ = "_id"
-- >  
-- >  data FirstName = FirstName deriving (Show, Eq)
-- >  instance Selectable User FirstName String where s _ _ = "firstName"
-- >  
-- >  data LastName = LastName deriving (Show, Eq)
-- >  instance Selectable User LastName String where s _ _ = "lastName"
-- 
deriveSelectable :: Name -> [VarStrictType] -> Q [Dec]
deriveSelectable conName vs = concat <$> (mapM (deriveSelectable' conName) vs)

-- | Given name of type, and field, generate new type corrsponding to
-- the field and make it an instance of @Selectable@.
deriveSelectable' :: Name -> VarStrictType -> Q [Dec]
deriveSelectable' conName (n,_,t) = do
  let bn = mkName . cap $ nameBase n
      sName = mkName "s"
  -- New type for field:
  [DataD _ _ _ _ derivs] <- [d| data Constr = Constr deriving (Eq, Show) |]
  let dataType = DataD [] bn [] [NormalC bn []] derivs
  -- Instance of Selectable:
  [InstanceD selCtx (AppT (AppT (AppT selT _) _) _)
                    [FunD _ [Clause pats (NormalB (AppE varE_u _)) []]]]
     <-  [d| instance Selectable T1 T2 T3 where
               s _ _ = (T.pack "")
         |]
  let lit = LitE .  StringL $ if is_id t then "_id" else nameBase n
      selInstance = 
        InstanceD selCtx (AppT (AppT (AppT selT (ConT conName)) (ConT bn)) t)
            [FunD sName
                   [Clause pats
                     (NormalB (AppE varE_u lit)) []
                   ]
            ]
  --
  return [dataType, selInstance]
    where cap (c:cs) = toUpper c : cs
          cap x = x
          is_id (ConT c)   = (c == ''SObjId)
          is_id (AppT _ _) = False
          is_id _          = error "Invalid usage of is_id_, expecting ConT or AppT"


