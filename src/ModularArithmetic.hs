{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications, KindSignatures, ScopedTypeVariables #-}

module ModularArithmetic where

import Control.Arrow ((***))
import Control.Lens
import Data.Function (on)
import Data.Proxy (Proxy (..))
import Data.Ratio ((%))
import GHC.TypeLits (Nat, natVal, KnownNat)

newtype Mod a (m :: Nat) = Mod a deriving (Eq, Ord)
type ValidMod a m = (Integral a, KnownNat m)

modLens :: ValidMod a m => Lens' (Mod a m) a
modLens = lens unMod $ const fromIntegral

unsafeLens :: ValidMod a m => Lens' (Mod a m) a
unsafeLens = lens unMod $ const Mod

unMod :: Mod a m -> a
unMod (Mod a) = a

modMap :: (a -> a) -> Mod a m -> Mod a m
modMap f = Mod . f . unMod

modVal :: forall a m. ValidMod a m => Mod a m
modVal = Mod $ fromInteger $ natVal $ Proxy @m

instance Show a => Show (Mod a m) where
	show = show . unMod

instance ValidMod a m => Enum (Mod a m) where
	fromEnum = fromIntegral . unMod
	toEnum = fromIntegral

instance ValidMod a m => Num (Mod a m) where
	(+) = (fromIntegral .) . ((+) `on` unMod)
	(*) = (fromIntegral .) . ((*) `on` unMod)
	negate = modLens %~ negate
	abs = id
	signum = id
	fromInteger = Mod . fromInteger . flip mod (natVal $ Proxy @m)

instance (Ord a, ValidMod a m) => Real (Mod a m) where
	toRational = toRational . toInteger . unMod

instance ValidMod a m => Integral (Mod a m) where
	quotRem = ((Mod *** Mod) .) . (quotRem `on` unMod)
	toInteger = toInteger . unMod