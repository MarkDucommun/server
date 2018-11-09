module Server.Response.Writer
  ( sendResponse
  ) where

import System.IO
import Server.Response.Types
import Server.Response.Transformer

sendResponse :: Handle -> IO Response -> IO ()
sendResponse handle response = do
  resp <- response
  writeResponse handle $ transformResponse resp

writeResponse :: Handle -> [String] -> IO ()
writeResponse handle [] = do
  hFlush handle
  hClose handle
writeResponse handle (line:lines) = do
  hPutStr handle line
  writeResponse handle lines