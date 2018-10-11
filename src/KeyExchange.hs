{-# LANGUAGE OverloadedStrings #-}

module KeyExchange (main) where

import Prelude hiding (putStrLn, null)
import qualified Prelude as P (putStrLn)
import Control.Concurrent (forkFinally)
import Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Data.ByteString.Char8 (putStrLn, pack, unpack)
import Data.ByteString (append, null)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import Control.Concurrent.MVar
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List ((\\))
import Data.Maybe (listToMaybe)

import DiffieHellman
import Polynomial

type ClientList = MVar (Map Client (Socket, Integer))

lookupKey :: ClientList -> Client -> IO Integer
lookupKey clientList client = withMVar clientList $ return . snd . (Map.! client)

newClientList :: IO ClientList
newClientList = newMVar Map.empty

nextClientIdentifier :: ClientList -> IO Client
nextClientIdentifier clientList = do
	clients <- readMVar clientList
	return $ head $ ['A'..'Z'] \\ Map.keys clients

acceptClient :: Socket -> ClientList -> IO Client
acceptClient sock clientList = do
	s <- diffieHellman sock
	client <- nextClientIdentifier clientList
	modifyMVar_ clientList $ return . Map.insert client (sock, s)
	sendIdentifier sock client
	return client

receiveClientFrom :: ClientList -> Socket -> IO (Client, Integer)
clientList `receiveClientFrom` sock = do
	client <- receiveIdentifier sock
	key <- lookupKey clientList client
	return (client, key)

main :: IO ()
main = withSocketsDo $ do
	clientList <- newClientList
	bracket open close $ loop clientList
	where
		resolve port = do
			let hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}
			fmap head $ getAddrInfo (Just hints) Nothing (Just port)
		open = do
			addr <- resolve "3000"
			sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
			setSocketOption sock ReuseAddr 1
			bind sock $ addrAddress addr
			-- If the prefork technique is not used,
			-- set CloseOnExec for the security reasons.
			setCloseOnExecIfNeeded $ fdSocket sock
			listen sock 10
			return sock
		loop clientList sock = forever $ do
			(conn, peer) <- accept sock
			putStrLn $ append "Connection from " (pack $ show peer)
			forkFinally (talk conn clientList) (const $ close conn)
		talk conn clientList = do
			client <- acceptClient conn clientList
			(idA, kAS) <- clientList `receiveClientFrom` conn
			(idB, kBS) <- clientList `receiveClientFrom` conn
			nonceA <- receiveNumber conn
			kAB <- return 4 -- Highly random number generated through super secret process
			let msgToBob = encrypt kBS $ pack $ (show kAB) ++ "|" ++ (show idA)
			--let msg = (show nonceA) ++ "|" ++ (show kAB) ++ (show idB)
			sendAll conn $ encrypt kAS "Hello World"
			return 0