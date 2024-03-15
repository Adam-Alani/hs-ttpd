module Main where

import Handler (runServer)
import Network.Socket (AddrInfo (addrFlags), AddrInfoFlag (AI_PASSIVE), defaultHints, getAddrInfo, withSocketsDo)
import System.Environment (getArgs)

main :: IO ()
main = withSocketsDo $ do
  let port = getArgs >>= \args -> if null args then return "3000" else return (head args)
  parsedPort <- port
  addrinfos <-
    getAddrInfo
      (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
      Nothing
      (Just parsedPort)

  case addrinfos of
    [] -> error "No address info, please try again."
    (serveraddr : _) -> do
      putStrLn $ "Listening on port " ++ parsedPort
      runServer serveraddr
