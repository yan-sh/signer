{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}

module Signer (Signed, wrap, unsign, sign, HasSign, HasSigns) where

import Data.Kind (Type, Constraint)
import Data.Coerce (coerce)

newtype Signed (a :: Type) (s :: [Type]) = Signed a

wrap :: a -> Signed a '[]
wrap = coerce
{-# INLINE wrap #-}

unsign :: Signed a s -> a
unsign = coerce
{-# INLINE unsign #-}

sign :: s -> Signed a ss -> Signed a (s ': ss)
sign s x = s `seq` coerce x
{-# INLINE sign #-}

type HasSign :: Type -> [Type] -> Constraint
type HasSign s ss = HasSignTF s ss ~ 'True

type HasSigns :: [Type] -> [Type] -> Constraint
type HasSigns ss1 ss2 = HasSignsTF ss1 ss2 ~ 'True

type family HasSignTF (s :: Type) (ss :: [Type]) :: Bool where
  HasSignTF s (s ': _) = 'True
  HasSignTF s (_ ': ss) = HasSignTF s ss 
  HasSignTF _ _ = 'False

type family And (b1 :: Bool) (b2 :: Bool) :: Bool where
  And 'True 'True = 'True
  And _ _  = 'False

type family HasSignsTF (ss1 :: [Type]) (ss2 :: [Type]) :: Bool where
  HasSignsTF (s ': ss1) ss2 = And (HasSignTF s ss2) (HasSignsTF ss1 ss2)
  HasSignsTF _ _ = 'True
