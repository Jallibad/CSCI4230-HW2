{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (bracket)
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
	bracket (open addr) close talk
	where
		resolve host port = do
			let hints = defaultHints {addrSocketType = Stream}
			fmap head $ getAddrInfo (Just hints) (Just host) (Just port)
		open addr = do
			sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
			connect sock $ addrAddress addr
			return sock
		talk sock = do
			s <- diffieHellman sock
			myIdentifier <- receiveIdentifier sock
			sendIdentifier sock myIdentifier
			msg <- recv sock 1024
			putStrLn $ decrypt s msg