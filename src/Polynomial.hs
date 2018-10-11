{-# LANGUAGE ViewPatterns #-}

module Polynomial where

import Control.Arrow (first)
import Data.Function (on)
import qualified Data.Map.Strict as Map
import Control.Lens
import Debug.Trace

newtype Polynomial a b = Polynomial (Map.Map a b) deriving Eq

getMap :: Polynomial a b -> Map.Map a b
getMap (Polynomial p) = p

getTerms :: Polynomial a b -> [(a,b)]
getTerms = Map.assocs . getMap

getExps :: Polynomial a b -> [a]
getExps = Map.keys . getMap

termWise :: (Num a, Num b, Num c, Eq c) =>
			(b -> c) ->
			Polynomial a b ->
			Polynomial a c
termWise f = normalize . fmap f . getMap

termWise' :: (Num a, Num b, Num c) =>
			(b -> c) ->
			Polynomial a b ->
			Polynomial a c
termWise' f = Polynomial . fmap f . getMap

instance (Integral a, Num b, Eq b) => Num (Polynomial a b) where
	(+) = (normalize .) . (Map.unionWith (+) `on` getMap)
	(Polynomial p1) * (Polynomial p2) = normalize $ Map.unionsWith (+) $ Map.foldlWithKey (\m c v -> (Map.mapKeysMonotonic (c+) $ fmap (v*) p2):m) [] p1
	abs = termWise' abs
	signum = Polynomial . Map.singleton 0 . signum . snd . Map.findMax . getMap
	fromInteger = Polynomial . Map.singleton 0 . fromInteger
	negate = termWise' negate

normalize :: (Num a, Num b, Eq b) => Map.Map a b -> Polynomial a b
normalize (Map.filter (/=0) -> m) = Polynomial $ if (Map.null m) then Map.singleton 0 0 else m

instance (Show a, Show b) => Show (Polynomial a b) where
	show = tail . Map.foldlWithKey (\s k a -> ' ':(show a)++"x^"++(show k)++s) "" . getMap

constructPolynomial :: (Enum a, Num a, Num b, Eq b) => [b] -> Polynomial a b
constructPolynomial = normalize . Map.fromDistinctAscList . zip [0..]

eval :: (Integral a, Num b) => Polynomial a b -> b -> b
eval p x = Map.foldrWithKey (\k a -> (x^k*a+)) 0 $ getMap p

polynomialDiv :: (Integral a, Eq b, Fractional b) =>
			Polynomial a b ->
			Polynomial a b ->
			(Polynomial a b, Polynomial a b)
polynomialDiv n d
	| n == 0 || degree n < degree d = (0, n)
	| otherwise = first (+t) $ polynomialDiv (n-t*d) d
	where t = (lead n) `divideTerm` (lead d)

polynomialRemainder :: (Integral a, Eq b, Fractional b) =>
			Polynomial a b ->
			Polynomial a b ->
			Polynomial a b
polynomialRemainder = (snd .) . polynomialDiv

--polynomialMod :: (Num a, Integral b) => Polynomial a b -> b -> Polynomial a b
p `polynomialMod` n = snd $ polynomialQuotRem p n

lead :: Polynomial a b -> (a, b)
lead = Map.findMax . getMap

degree :: Polynomial a b -> a
degree = fst . lead

divideTerm :: (Num a, Fractional b) => (a,b) -> (a,b) -> Polynomial a b
divideTerm (a,b) (c,d) = Polynomial $ Map.singleton (a-c) (b/d)

polynomialQuotRem :: (Integral a, Integral b) =>
			Polynomial a b ->
			Polynomial a b ->
			(Polynomial a b, Polynomial a b)
polynomialQuotRem n d
	| n == 0 || degree n < degree d = (0, n)
	| otherwise = first (+t) $ polynomialQuotRem (n-t*d) d
	where
		t = (lead n) `divideTerm` (lead d)
		divideTerm (a,b) (c,d) = Polynomial $ Map.singleton (a-c) (b `div` d)

x :: (Num a, Num b) => Polynomial a b
x = Polynomial $ Map.singleton 1 1