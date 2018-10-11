{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception
import Data.ByteString.Char8 hiding (head)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import Prelude hiding (putStrLn)
import qualified Prelude as P (putStrLn)

import DiffieHellman
import ModularArithmetic

resolve :: Maybe HostName -> ServiceName -> IO AddrInfo
resolve host port = do
	let hints = defaultHints {addrSocketType = Stream}
	fmap head $ getAddrInfo (Just hints) host (Just port)

open :: AddrInfo -> IO Socket
open addr = do
	sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
	connect sock $ addrAddress addr
	return sock

getKey :: IO (Integer, Client)
getKey = do
	addr <- resolve (Just "127.0.0.1") "3000"
	bracket (open addr) close talk
	where
		talk sock = do
			s <- diffieHellman sock
			myIdentifier <- receiveIdentifier sock
			return (s, myIdentifier)

listenForAlice :: IO Socket
listenForAlice = do
	let hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}
	addr:_ <- getAddrInfo (Just hints) Nothing (Just "3001")
	sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
	setSocketOption sock ReuseAddr 1
	bind sock $ addrAddress addr
	setCloseOnExecIfNeeded $ fdSocket sock
	listen sock 1
	(conn, _) <- accept sock
	close sock
	return conn

main = withSocketsDo $ do
	(s, myIdentifier) <- getKey
	print myIdentifier
	sock <- listenForAlice
	sendIdentifier sock myIdentifier