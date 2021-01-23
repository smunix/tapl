{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}

-- |
module Chap03.Sec05.Arithmetic where

import qualified Chap03.Sec05.Boolean as Boolean
import Control.Monad.Fix (fix)

data Term where
  Bottom :: Term
  Value :: Value -> Term
  Succ :: Term -> Term
  Pred :: Term -> Term
  IsZero :: Term -> Term
  deriving (Eq, Ord, Show)

data Value where
  Zero :: Value
  Boolean :: Boolean.Term -> Value
  deriving (Eq, Ord, Show)

step :: Term -> Term
step !t =
  if
      | Pred (Value Zero) <- t -> Value Zero
      | IsZero (Value Zero) <- t -> Value $ Boolean Boolean.T
      | IsZero (Succ t') <- t -> IsZero (Succ (step t'))
      | Pred (Succ t') <- t -> t'
      | Pred t' <- t -> Pred (step t')
      | Succ (Pred t') <- t -> t'
      | Succ t' <- t -> Succ (step t')
      | otherwise -> Bottom

step' :: (Term -> Term -> Bool) -> Term -> Term
step' = fix \rec stop !t ->
  let !t' = step t
   in if stop t t'
        then t'
        else rec stop t'

-- Seq.iterateN 5 Arith.step (Succ $ Succ $ Pred $ Succ $ Pred $ Succ $ Pred $ Value $ Boolean T)
