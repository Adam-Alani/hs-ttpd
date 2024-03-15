module Handler where

import Control.Concurrent (forkIO)
import Data.String (IsString (fromString))
import DataTypes (Request (path), Response (Response, resp_body, status), StatusCode (InternalServerError, NotFound, OK), serialize)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Directory
import System.IO

pathRoot :: String
pathRoot = "./server"

runServer :: AddrInfo -> IO ()
runServer serveraddr = do
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 5
  acceptLoop sock

acceptLoop :: Socket -> IO ()
acceptLoop sock = do
  (conn, _) <- accept sock
  putStrLn "Accepted connection "
  _ <- forkIO $ handleConn conn
  acceptLoop sock

handleConn :: Socket -> IO ()
handleConn conn = do
  req <- recv conn 1024

  let request = serialize (show req)
  case request of
    Nothing -> do
      sendAll conn (fromString (show Response {status = InternalServerError, resp_body = Nothing}))
    Just req_data -> do
      handleRequest req_data conn

  close conn

handleRequest :: Request -> Socket -> IO ()
handleRequest req conn = do
  let path' = pathRoot ++ path req

  fileExists <- doesFileExist path'
  if fileExists
    then do
      handle <- openFile path' ReadMode
      contents <- hGetContents handle
      print contents
      let response = Response {status = OK, resp_body = Just contents}
      sendAll conn (fromString (show response))
    else do
      let response = Response {status = NotFound, resp_body = Nothing}
      sendAll conn (fromString (show response))
      print $ show response
