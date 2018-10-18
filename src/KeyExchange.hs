{-# LANGUAGE OverloadedStrings		#-}
{-# LANGUAGE ViewPatterns			#-}

module KeyExchange (main) where

import Prelude hiding (putStrLn, null)
import qualified Prelude as P (putStrLn)
import Control.Concurrent (forkFinally)
import Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Data.ByteString.Char8 (putStrLn, pack, unpack)
import Data.ByteString (ByteString, append, null)
import Network.Socket hiding (recv, send)
import Control.Concurrent.MVar
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List ((\\))
import Data.Maybe (listToMaybe)
import Data.Serialize

import DiffieHellman
import Polynomial

type ClientList = MVar (Map Client (Socket, GF2))

lookupKey :: ClientList -> Client -> IO GF2
lookupKey clientList client = withMVar clientList $ return . snd . (Map.! client)

newClientList :: IO ClientList
newClientList = newMVar Map.empty

nextClientIdentifier :: ClientList -> IO Client
nextClientIdentifier clientList = do
	clients <- readMVar clientList
	return $ head $ ['A'..'Z'] \\ Map.keys clients

acceptClient :: Socket -> ClientList -> IO ()
acceptClient sock clientList = do
	s <- diffieHellman' sock
	client <- nextClientIdentifier clientList
	modifyMVar_ clientList $ return . Map.insert client (sock, s)
	send sock client

receiveClientFrom :: ClientList -> Socket -> IO (Client, GF2)
clientList `receiveClientFrom` sock = do
	client <- receive sock
	key <- lookupKey clientList client
	return (client, key)

talk :: Socket -> ClientList -> IO ()
talk sock clientList = do
		op <- receive sock
		case op of
			CreateServerConnection -> acceptClient sock clientList
			GetSharedKey -> do
				(idA, idB, nonceA, bobMessage) <- receive sock :: IO (Client, Client, Nonce, ByteString)
				kAS <- lookupKey clientList idA
				kBS <- lookupKey clientList idB
				let kAB = x^5+x^2+1 :: GF2
				let ((==idA) -> True, nonceB) = decodeEncrypted kBS bobMessage
				let newBobMessage = encrypt kBS $ encode (kAB, idA, nonceB)
				sendEncrypted sock kAS (nonceA, kAB, idB, newBobMessage)
				return ()
		return ()

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