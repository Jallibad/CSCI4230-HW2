{-# LANGUAGE OverloadedStrings #-}

module DiffieHellman where

import Data.ByteString (append)
import Data.ByteString.Char8 (putStrLn, pack, unpack, head, singleton)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)
import Prelude hiding (putStrLn, head)
import System.Random

type Client = Char

p :: Integer
p = 23
g :: Integer
g = 5

diffieHellman :: Socket -> IO Integer
diffieHellman sock = do
	a <- getStdRandom $ randomR (1,p) :: IO Integer
	--putStrLn $ append "a:" $ pack $ show a
	sendNumber sock $ (g^a) `mod` p
	capB <- receiveNumber sock
	return $ (capB^a) `mod` p

receiveNumber :: Socket -> IO Integer
receiveNumber sock = do
	msg <- recv sock 1024
	return $ read $ unpack msg

sendNumber :: Socket -> Integer -> IO ()
sendNumber sock n = sendAll sock $ pack $ show n

receiveIdentifier :: Socket -> IO Client
receiveIdentifier sock = do
	msg <- recv sock 1
	return $ head msg

sendIdentifier :: Socket -> Client -> IO ()
sendIdentifier sock i = sendAll sock $ singleton i