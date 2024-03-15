module Main where

import Control.Concurrent (forkIO)
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import Network.Socket (AddrInfo (addrAddress, addrFamily, addrFlags), AddrInfoFlag (AI_PASSIVE), Socket, SocketType (Stream), accept, bind, close, defaultHints, defaultProtocol, getAddrInfo, listen, socket, withSocketsDo)
import Network.Socket.ByteString (recv, sendAll)
import System.Directory
import System.IO

data Request = Request
  { method :: !String,
    path :: !String,
    headers :: ![(String, String)],
    body :: !String
  }
  deriving (Show)

data Response = Response
  { status :: !Int,
    resp_body :: !(Maybe String)
  }

instance Show Response where
  show r = "HTTP/1.1 " ++ show (status r) ++ " " ++ statusMessage (status r) ++ "\r\n\r\n" ++ fromMaybe "" (resp_body r)

statusMessage :: Int -> String
statusMessage 200 = "OK"
statusMessage 404 = "Not Found"
statusMessage 500 = "Internal Server Error"
statusMessage _ = "Unknown Status Code"

serialize :: String -> Maybe Request
serialize requestStr = do
  (headerLines, bodyLines) <- splitAtHeaderAndBody requestStr
  let maybeHeaders = map parseHeader headerLines
  case sequence maybeHeaders of
    Nothing -> Nothing
    Just parsedHeaders -> do
      let headers' = parsedHeaders
      let methodLine = head headerLines
      let method' = takeWhile (/= ' ') methodLine
      let pathLine = words methodLine !! 1
      let path' = takeWhile (/= ' ') pathLine
      return Request {method = method', path = path', headers = headers', body = unlines bodyLines}

parseHeader :: String -> Maybe (String, String)
parseHeader line =
  let (name, value) = break (== ':') line
   in Just (name, dropWhile (== ' ') (drop 1 value))

splitAtHeaderAndBody :: String -> Maybe ([String], [String])
splitAtHeaderAndBody requestStr =
  let (headerStr, bodyStr) = break (== "\r\n\r\n") (lines requestStr)
   in Just (headerStr, drop 1 bodyStr)

main :: IO ()
main = withSocketsDo $ do
  addrinfos <-
    getAddrInfo
      (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
      Nothing
      (Just "3000")

  case addrinfos of
    [] -> error "No address info, please try again."
    (serveraddr : _) -> runServer serveraddr

runServer :: AddrInfo -> IO ()
runServer serveraddr = do
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 5

  putStrLn "Listening on port 3000"
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
  print req

  let request = serialize (show req)
  print request
  case request of
    Nothing -> do
      let response = Response {status = 500, resp_body = Just $ statusMessage 500}
      sendAll conn (fromString (show response))
      print $ show response
    Just req_data -> do
      handleRequest req_data conn

  close conn

handleRequest :: Request -> Socket -> IO ()
handleRequest req conn = do
  let path' = "./server" ++ path req

  fileExists <- doesFileExist path'
  if fileExists
    then do
      handle <- openFile path' ReadMode
      contents <- hGetContents handle
      print contents
      let response = Response {status = 200, resp_body = Just contents}
      sendAll conn (fromString (show response))
    else do
      let response = Response {status = 404, resp_body = Just $ statusMessage 404}
      sendAll conn (fromString (show response))
      print $ show response
