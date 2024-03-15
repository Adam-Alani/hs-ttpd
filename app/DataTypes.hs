module DataTypes where

import Data.Maybe (fromMaybe)

data Request = Request
  { method :: !String,
    path :: !String,
    headers :: ![(String, String)],
    body :: !String
  }
  deriving (Show)

data Response = Response
  { status :: !StatusCode,
    resp_body :: !(Maybe String)
  }

instance Show Response where
  show r = "HTTP/1.1 " ++ show (status r) ++ "\r\n\r\n" ++ fromMaybe "" (resp_body r)

data StatusCode = OK | NotFound | InternalServerError | UnknownStatusCode !Int

instance Show StatusCode where
  show OK = "200 OK"
  show NotFound = "404 Not Found"
  show InternalServerError = "500 Internal Server Error"
  show (UnknownStatusCode code) = show code ++ " Unknown Status Code"

serialize :: String -> Maybe Request
serialize requestStr = do
  (headerLines, bodyLines) <- splitAtHeaderAndBody requestStr
  headers' <- mapM parseHeader headerLines
  case headerLines of
    [] -> Nothing
    (methodLine : _) -> do
      let method' = takeWhile (/= ' ') methodLine
      case words methodLine of
        [] -> Nothing
        [_] -> Nothing
        (_ : pathLine : _) -> do
          let path' = takeWhile (/= ' ') pathLine
          return Request {method = method', path = path', headers = headers', body = unlines bodyLines}

-- | Splits a request string into a header and a body.
splitAtHeaderAndBody :: String -> Maybe ([String], [String])
splitAtHeaderAndBody requestStr =
  let (headerStr, bodyStr) = break (== "\r\n\r\n") (lines requestStr)
   in Just (headerStr, drop 1 bodyStr)

-- | Parses a header line into a key-value pair.
parseHeader :: String -> Maybe (String, String)
parseHeader line =
  let (name, value) = break (== ':') line
   in Just (name, dropWhile (== ' ') (drop 1 value))
