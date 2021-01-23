{-# LANGUAGE GADTs #-}

-- |
module Chap03.Sec02.Def03 where

import Control.Monad (join)
import qualified Data.Set as Set
import Lens.Micro.Platform (traverseOf, traversed, (&), (<&>), (^.))

data Expr where
  T :: Expr
  F :: Expr
  Z :: Expr
  Succ :: Expr -> Expr
  Pred :: Expr -> Expr
  IsZero :: Expr -> Expr
  If :: Expr -> Expr -> Expr -> Expr
  deriving (Eq, Ord, Show)

s0 :: Set.Set Expr
s0 = Set.empty

terms :: Set.Set Expr -> Set.Set Expr
terms s = baseTerms `Set.union` unaryTerms `Set.union` ifTerms
  where
    s' :: [] Expr
    s' = Set.toList s

    baseTerms :: Set.Set Expr
    baseTerms = Set.fromList [Z, T, F]

    ifTerms :: Set.Set Expr
    ifTerms = If <$> s' <*> s' <*> s' & Set.fromList

    unaryTerms :: Set.Set Expr
    unaryTerms =
      s'
        & traverseOf
          traversed
          ((<&>) [Succ, Pred, IsZero] . flip ($))
        & join
        & Set.fromList
