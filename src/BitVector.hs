{-# LANGUAGE ConstraintKinds		#-}
{-# LANGUAGE DataKinds				#-}
{-# LANGUAGE FlexibleContexts		#-}
{-# LANGUAGE KindSignatures			#-}
{-# LANGUAGE RankNTypes				#-}
{-# LANGUAGE ScopedTypeVariables	#-}
{-# LANGUAGE TypeApplications		#-}
{-# LANGUAGE TypeFamilies			#-}
{-# LANGUAGE TypeOperators			#-}
{-# LANGUAGE UndecidableInstances	#-}
{-# LANGUAGE ViewPatterns			#-}

module BitVector (
	-- * Types
	BitVector,
	Thing,
	Addition,
	-- * Functions
	-- ** Construction
	-- ** Lenses
	asVec,
	asIntegral,
	asList,
	digitWise,
	-- ** Size Modifying Functions
	BitVector.concat,
	BitVector.concat',
	split,
	-- ** Miscellaneous
	backPermute,
	backPermute',
	fromList,
	toList
) where

import Control.Arrow ((***))
import Control.Lens
import Data.Bits
import Data.Char (intToDigit)
import Data.Function (on)
import Data.Monoid (Sum)
import Data.Ord (comparing)
import Data.Proxy
import Data.Ratio ((%))
import Data.Vector.Fixed.Unboxed (Vec)
import Data.Vector.Fixed.Cont hiding (map, fromList, toList)
import Data.Vector.Fixed as Vec hiding (map, fromList, toList)
import qualified Data.Vector.Fixed as Vec (map, fromList, toList)
import GHC.TypeLits
import Numeric (showIntAtBase)

-- |The basic datatype of the BitVector module, represents a vector of booleans with the static size `n`
newtype BitVector (n :: Nat) = BitVector {_vec :: Vec n Bool}

type Thing a = (Peano (a + 1) ~ S (Peano a), KnownNat a, ArityPeano (Peano a))
type Addition a b = Peano (a+b) ~ Add (Peano a) (Peano b)

-- |Lens that converts the 'BitVector' to a 'Data.Vector.Fixed.Unboxed.Vec' n 'Bool'.
asVec :: Lens' (BitVector n) (Vec n Bool)
asVec = lens unwrap $ const BitVector

-- |Lens that converts the 'BitVector' to any 'Integral' a
asIntegral :: (Integral a, Thing n) => Lens' (BitVector n) a
asIntegral = lens fromIntegral $ const fromIntegral

asList :: Thing n => Lens' (BitVector n) [Bool]
asList = lens toList $ const fromList

digitWise :: Thing n => Setter' (BitVector n) Bool
digitWise = setting $ (asVec %~) . Vec.map

--digitsFold :: Thing n => Fold (BitVector n) Bool
--digitsFold = folding unwrap

unwrap :: BitVector n -> Vec n Bool
unwrap (BitVector v) = v

using :: (a -> a -> a) -> Lens' (BitVector n) a -> BitVector n -> BitVector n -> BitVector n
using f l = over l . f . view l

instance Thing a => Eq (BitVector a) where
	(==) = (==) `on` (^. asVec)

instance Thing a => Show (BitVector a) where
	show = ("0b"++) . Prelude.map (\b -> if b then '1' else '0') . BitVector.toList

instance Thing a => Num (BitVector a) where
	(+) = (+) `using` asIntegral
	(*) = (*) `using` asIntegral
	(-) = (-) `using` asIntegral
	abs = id
	signum _ = 1
	negate = error "Negation is not defined for boolean vectors"
	fromInteger = BitVector . Vec.reverse . Vec.generate . testBit

instance Thing a => Bits (BitVector a) where
	(.&.) = Vec.zipWith (.&.) `using` asVec
	(.|.) = Vec.zipWith (.|.) `using` asVec
	xor = Vec.zipWith xor `using` asVec
	complement = digitWise %~ not
	shift b i = thing (\((+i) -> n) -> n >= 0 && n < BitVector.length b && testBit b n) b
	rotate b i = fromList $ map
					(\n -> testBit b $ (n+i) `mod` len)
					[0.. len-1]
		where len = BitVector.length b
	bit = bitDefault
	testBit = (Vec.!) . (^. asVec)
	bitSizeMaybe = Just . BitVector.length
	bitSize = BitVector.length
	isSigned _ = False
	popCount = popCountDefault

instance Thing a => Integral (BitVector a) where
	toInteger = Vec.foldl (\n d -> 2*n + if d then 1 else 0) 0 . unwrap
	quotRem = ((fromInteger *** fromInteger) .) . on quotRem fromIntegral

instance Thing a => Real (BitVector a) where
	toRational = (%1) . toInteger

instance Thing a => Enum (BitVector a) where
	toEnum = fromIntegral
	fromEnum = fromIntegral

instance Thing a => Ord (BitVector a) where
	compare = comparing toInteger

length :: forall b. KnownNat b => BitVector b -> Int
length = const $ fromIntegral $ natVal $ Proxy @b

fromList :: Thing a => [Bool] -> BitVector a
fromList = BitVector . Vec.fromList

toList :: Thing a => BitVector a -> [Bool]
toList = Vec.toList . (^. asVec)

split :: forall a b c. (Thing a, Thing b, Thing c, a ~ (b+c)) => BitVector a -> (BitVector b, BitVector c)
split =	(fromList *** fromList)
		. splitAt (fromIntegral $ natVal $ Proxy @b)
		. BitVector.toList

-- |Concatenates two 'BitVector's of size @a@ and @b@ resulting in a new 'BitVector' with size @a+b ~ c@
concat :: (Thing a, Thing b, Thing c, Addition a b, (a+b) ~ c) => BitVector a -> BitVector b -> BitVector c
concat a b = BitVector $ Vec.concat (a ^. asVec) (b ^. asVec)

-- |Uncurried version of 'concat'
concat' :: (Thing a, Thing b, Addition a a, (a+a) ~ b) => (BitVector a, BitVector a) -> BitVector b
concat' = uncurry BitVector.concat

backPermute :: (Thing a, Thing b) => Vec b Int -> BitVector a -> BitVector b
backPermute is = BitVector . flip Vec.map is . testBit

backPermute' :: (Thing a, Thing b) => [Int] -> BitVector a -> BitVector b
backPermute' i b
	| Prelude.all (< BitVector.length b) i = fromList $ Prelude.map (testBit b) i
	| otherwise = error $ "backPermute' index(es) out of bounds: "++(show invalidIs)
	where invalidIs = Prelude.filter (>= BitVector.length b) i

thing :: (Thing a, Thing b) => (Int -> Bool) -> BitVector a -> BitVector b
thing f b = fromList $ map f [0.. BitVector.length b - 1]