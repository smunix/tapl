{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}

-- | Boolean evaluation rules
module Chap03.Sec05.Boolean where

import Control.Monad.Fix (fix)

-- | Boolean term definition
data Term where
  T ::
    -- | True
    Term
  F ::
    -- | False
    Term
  If ::
    Term ->
    Term ->
    Term ->
    -- | If Cond Then Tbranch else Fbranch
    Term
  deriving (Eq, Ord, Show)

step :: Term -> Term
step t =
  if
      | If T tbranch _ <- t -> tbranch
      | If F _ fbranch <- t -> fbranch
      | If c tbranch fbranch <- t -> If (step c) tbranch fbranch
      | otherwise -> t -- t is a final term

step' :: (Term -> Term -> Bool) -> Term -> Term
step' = fix \rec stop !t ->
  let !t' = step t
   in if
          | True <- stop t' t -> t
          | otherwise -> rec stop t'

-- | multi-step evaluation with monadic observations
step'' :: Term -> (Term -> Term -> Either (IO ()) (IO ())) -> IO () -> IO ()
step'' = fix \rec !t cb !r -> do
  let !t' = step t
  case cb t t' of
    Left r' -> r >> r'
    Right r' -> rec t' cb (r >> r')
