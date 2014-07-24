import Control.Concurrent (forkIO)
import Network (connectTo, PortID(..))
import System.Environment (getArgs)
import Control.Monad (forever)
import System.IO (hSetBuffering, BufferMode(..), hGetLine, hPutStrLn)
import GHC.IO.Handle (Handle)

ping :: String
ping = "PING"

pong :: String
pong = "POMG"

startsWith :: String -> String -> Bool
startsWith [] [] = True
startsWith [] _ = True
startsWith _ [] = False
startsWith (p:ps) (s:xs) = (p == s) && (startsWith ps xs)

handleReceived :: Handle -> String -> IO ()
handleReceived h s = if (startsWith "PING" s)
                          then hPutStrLn h $ makeResponse s
                          else putStrLn s

makeResponse :: String -> String
makeResponse = unwords . (pong :) . drop 1 . words

listener :: Handle -> IO ()
listener h = do
	msg <- hGetLine h
	handleReceived h msg

identifyString :: String
identifyString = "/id"

joinString :: String
joinString = "/j"

performIdentify :: Handle -> String -> IO ()
performIdentify h line = do
  let nick = drop 1 $ words line
  putStrLn $ unwords nick
  hPutStrLn h $ unwords $ "NICK ":nick
  hPutStrLn h "USER noone 8 *  :Just Nobody"

performJoin :: Handle -> String -> IO ()
performJoin h line = do
  let command = unwords $ ("JOIN" :) $ drop 1 $ words line
  hPutStrLn h command

handleCommand :: Handle -> String -> IO ()
handleCommand h line
  | startsWith identifyString line = performIdentify h line
  | startsWith joinString line = performJoin h line
  | otherwise = hPutStrLn h line

responder :: Handle -> IO ()
responder h = do
  line <- getLine
  handleCommand h line

launch :: String -> PortID -> IO ()
launch host port = do
	h <- connectTo host port
	hSetBuffering h NoBuffering
	t <- forkIO $ forever $ listener h
	forever $ responder h

displayPrompt :: IO ()
displayPrompt = putStrLn "Usage: not implemented yet"

ircPort :: PortID
ircPort = PortNumber 6667

main :: IO ()
main = do
  args <- getArgs
  case args of
    (address : _) -> launch address ircPort
    _ -> displayPrompt
