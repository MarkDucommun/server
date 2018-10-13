module Server
  ( startServer
  , PortNumber
  ) where

import           Network
import           System.IO

startServer :: PortID -> IO ()
startServer port = withSocketsDo $ do
  sock <- listenOn port
  loop sock

loop :: Socket -> IO()
loop socket = do
  (handle, _, _) <- accept socket
  line <- hGetLine handle
  hPutStr handle line
  hFlush handle
  hClose handle
  loop socket