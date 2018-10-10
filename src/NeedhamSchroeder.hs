{-# LANGUAGE OverloadedStrings #-}
-- Echo client program
module NeedhamSchroeder (main) where

import qualified Control.Exception as E
import Data.ByteString.Char8 as C
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import Prelude hiding (putStr, putStrLn)

main :: IO ()
main = withSocketsDo $ do
	E.bracket open close talk
  where
	resolve host port = do
		let hints = defaultHints
		addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
		return addr
	open = do
		let hints = defaultHints { addrSocketType = Stream }
		addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "3000")
		sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
		connect sock $ addrAddress addr
		return sock
	talk sock = do
		sendAll sock "Hello, world!"
		msg <- recv sock 1024
		putStr "Received: "
		putStrLn msg