{-# LANGUAGE DataKinds				#-}
{-# LANGUAGE FlexibleInstances		#-}
{-# LANGUAGE OverloadedStrings		#-}
{-# LANGUAGE TypeSynonymInstances	#-}
{-# LANGUAGE ViewPatterns			#-}

module DiffieHellman where

import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Data.ByteString (ByteString, append)
import Data.ByteString.Char8 (putStrLn, pack, unpack, head, singleton)
import Data.Either (fromRight)
import Data.Map.Strict ((!?))
import Data.Maybe (fromMaybe)
import Data.Serialize
import ModularArithmetic
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)
import Polynomial
import Prelude hiding (putStrLn, head)
import System.Random
import ToyDES (encrypt', decrypt')

type Client = Char
type Nonce = Integer
type GF2 = Polynomial Integer (Mod Int 2)

instance (Ord a, Serialize a, Serialize b) => Serialize (Polynomial a b) where
	put = put . getMap
	get = liftM Polynomial get

instance ValidMod a m => Serialize (Mod a m) where
	put = put . toInteger
	get = liftM fromInteger get

encrypt :: GF2 -> ByteString -> ByteString
encrypt = encrypt' . sum . map (2^) . getExps

decrypt :: GF2 -> ByteString -> ByteString
decrypt = decrypt' . sum . map (2^) . getExps

p :: Integer
p = 23
g :: Integer
g = 5

p' :: GF2
p' = x^7+x+1
g' :: GF2
g' = x

modularPow :: (Integral a, Integral b) => a -> b -> a -> a
modularPow _ 0 _ = 1
modularPow b e m = if odd e then mult result b else result
	where
		result = modularPow (mult b b) (e `div` 2) m
		mult x y = (x*y) `mod` m

diffieHellman :: Socket -> IO Integer
diffieHellman sock = do
	a <- getStdRandom $ randomR (1,p) :: IO Integer
	--putStrLn $ append "a:" $ pack $ show a
	send sock $ modularPow g a p
	capB <- receive sock
	return $ (capB^a) `mod` p

diffieHellman' :: Socket -> IO GF2
diffieHellman' sock = do
	a <- getStdRandom $ randomR (1,10^2) :: IO Integer
	let capA = (g' ^ a) `polynomialMod` p'
	send sock capA
	capB <- receive sock
	return $ (capB^a) `polynomialMod` p'

send :: Serialize a => Socket -> a -> IO ()
send sock = sendAll sock . encode

sendEncrypted :: Serialize a => Socket -> GF2 -> a -> IO ()
sendEncrypted sock key = sendAll sock . encrypt key . encode

receive :: Serialize a => Socket -> IO a
receive sock = recv sock 4096 >>= either fail return . decode

decodeEncrypted :: Serialize a => GF2 -> ByteString -> a
decodeEncrypted key string = fromRight $ decode $ decrypt key string

{-
receiveNumber :: Socket -> IO Integer
receiveNumber sock = fmap (read . unpack) $ recv sock 1024

sendNumber :: Socket -> Integer -> IO ()
sendNumber sock n = sendAll sock $ pack $ show n

receivePolynomial :: Socket -> IO GF2
receivePolynomial sock = recv sock 4096 >>= either fail return . decode

sendPolynomial :: Socket -> GF2 -> IO ()
sendPolynomial sock = sendAll sock . encode

receiveIdentifier :: Socket -> IO Client
receiveIdentifier sock = do
	msg <- recv sock 1
	return $ head msg

sendIdentifier :: Socket -> Client -> IO ()
sendIdentifier sock i = sendAll sock $ singleton i
-}

data Operation = CreateServerConnection | GetSharedKey | CloseConnection deriving (Enum)

--instance Serialize Operation where