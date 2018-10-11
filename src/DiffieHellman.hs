{-# LANGUAGE OverloadedStrings	#-}
{-# LANGUAGE DataKinds			#-}

module DiffieHellman where

import Data.ByteString (ByteString, append)
import Data.ByteString.Char8 (putStrLn, pack, unpack, head, singleton)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)
import Prelude hiding (putStrLn, head)
import System.Random
import ToyDES (encrypt', decrypt')

import ModularArithmetic
import Polynomial

type Client = Char
type Nonce = Integer
type GF2 = Polynomial Integer (Mod Int 2)

encrypt :: Integer -> ByteString -> ByteString
encrypt n p = encrypt' (fromInteger n) p

decrypt :: Integer -> ByteString -> ByteString
decrypt n p = decrypt' (fromInteger n) p

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
	sendNumber sock $ modularPow g a p
	capB <- receiveNumber sock
	return $ (capB^a) `mod` p

diffieHellman' :: Socket -> IO GF2
diffieHellman' sock = do
	a <- getStdRandom $ randomR (1,10^2) :: IO Integer
	let capA = (g' ^ a) `polynomialMod` p'
	print capA
	sendPolynomial sock capA
	capB <- receivePolynomial sock
	print capB
	return $ (capB^a) `polynomialMod` p'

receiveNumber :: Socket -> IO Integer
receiveNumber sock = fmap (read . unpack) $ recv sock 1024

sendNumber :: Socket -> Integer -> IO ()
sendNumber sock n = sendAll sock $ pack $ show n

receivePolynomial :: Socket -> IO GF2
receivePolynomial sock = do
	print "RECEIVING POLYNOMIAL"
	exp <- receiveNumber sock
	print exp
	print "\n"
	if exp == -1
		then return 0
		else do
			rest <- receivePolynomial sock
			return $ x^exp+rest

sendPolynomial :: Socket -> GF2 -> IO ()
sendPolynomial sock p = do
	mapM_ (\exp -> print exp >> sendNumber sock exp) $ getExps p
	sendNumber sock (-1)

receiveIdentifier :: Socket -> IO Client
receiveIdentifier sock = do
	msg <- recv sock 1
	return $ head msg

sendIdentifier :: Socket -> Client -> IO ()
sendIdentifier sock i = sendAll sock $ singleton i