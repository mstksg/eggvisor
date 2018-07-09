{-# LANGUAGE FlexibleContexts                     #-}
{-# LANGUAGE FlexibleInstances                    #-}
{-# LANGUAGE GADTs                                #-}
{-# LANGUAGE KindSignatures                       #-}
{-# LANGUAGE MultiParamTypeClasses                #-}
{-# LANGUAGE PartialTypeSignatures                #-}
{-# LANGUAGE PolyKinds                            #-}
{-# LANGUAGE ScopedTypeVariables                  #-}
{-# LANGUAGE TypeApplications                     #-}
{-# LANGUAGE TypeInType                           #-}
{-# OPTIONS_GHC -fno-warn-orphans                 #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Data.Singletons.Util (
  ) where

import           Data.Constraint.Forall
import           Data.Dependent.Sum
import           Data.GADT.Compare
import           Data.GADT.Show
import           Data.Kind
import           Data.Singletons
import           Data.Singletons.Decide
import           Data.Singletons.Prelude.Ord
import qualified Data.Constraint             as C

instance (SingKind k, Show (Demote k)) => GShow (Sing :: k -> Type) where
    gshowsPrec p = showsPrec p . fromSing
instance SDecide k => GEq (Sing :: k -> Type) where
    geq x y = case x %~ y of
        Proved Refl -> Just Refl
        Disproved _ -> Nothing
instance (SDecide k, SOrd k) => GCompare (Sing :: k -> Type) where
    gcompare x y = case sCompare x y of
      SLT -> GLT
      SEQ -> case geq x y of
        Nothing   -> error "something wrong with SOrd instance"
        Just Refl -> GEQ
      SGT -> GGT

instance (SingKind k, Show (Demote k), ForallF Show f) => ShowTag (Sing :: k -> Type) f where
    showTaggedPrec (_ :: _ a) = showsPrec C.\\ instF @Show @f @a
instance (SDecide k, ForallF Eq f) => EqTag (Sing :: k -> Type) f where
    eqTagged      (_ :: _ a) _ x y = x == y C.\\ instF @Eq @f @a
instance (SDecide k, SOrd k, ForallF Ord f, ForallF Eq f) => OrdTag (Sing :: k -> Type) f where
    compareTagged (_ :: _ a) _ x y = compare x y C.\\ instF @Ord @f @a
