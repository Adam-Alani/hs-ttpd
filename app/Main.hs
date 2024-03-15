module Main where

import Handler (runServer)
import Network.Socket (AddrInfo (addrFlags), AddrInfoFlag (AI_PASSIVE), defaultHints, getAddrInfo, withSocketsDo)
import System.Environment (getArgs)

main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  case args of
    [] -> putStrLn "Please provide a port number."
    (firstArg : _) -> do
      addrinfos <-
        getAddrInfo
          (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
          Nothing
          (Just firstArg)

      case addrinfos of
        [] -> error "No address info, please try again."
        (serveraddr : _) -> do
          putStrLn $ "Listening on port " ++ firstArg
          runServer serveraddr
