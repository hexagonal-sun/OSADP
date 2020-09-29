module Main where

import Network.Socket
import Network.Socket.ByteString
import Data.ByteString as B
import Data.ByteString.UTF8
import Text.XML.Light.Input
import Text.XML.Light.Types
import Text.XML.Light.Proc

import qualified Commands as Commands
import Device

getReplies :: Socket -> IO ()
getReplies socket = do
  s <- recvFrom socket 2048
  let dev = deviceFromProbeResponse $ fst s
  case dev of
    Just d -> printDevice d
    Nothing -> print "Warning: unable to parse device response"
  getReplies socket

main :: IO ()
main = withSocketsDo $ do
  sock <- socket AF_INET Datagram 0
  let addr = SockAddrInet 37020
  probe <- Commands.probe
  Prelude.putStrLn "Searching for devices..."
  sendAllTo sock (fromString probe) (SockAddrInet 37020 $ tupleToHostAddress (239,255,255,250))
  getReplies sock
