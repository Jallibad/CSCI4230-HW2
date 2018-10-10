{-# LANGUAGE OverloadedStrings #-}
-- Echo client program
module Main (main) where

import qualified Control.Exception as E
import Data.ByteString.Char8
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import Prelude hiding (putStrLn)
import qualified Prelude as P (putStrLn)

import DiffieHellman
import ModularArithmetic

main :: IO ()
main = withSocketsDo $ do
	addr <- resolve "127.0.0.1" "3000"
	E.bracket (open addr) close talk
	where
		resolve host port = do
			let hints = defaultHints { addrSocketType = Stream }
			addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
			return addr
		open addr = do
			sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
			connect sock $ addrAddress addr
			return sock
		talk sock = do
			s <- diffieHellman sock
			P.putStrLn $ show s
			myIdentifier <- receiveIdentifier sock
			P.putStrLn [myIdentifier]