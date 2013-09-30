import Network (listenOn, accept,  withSocketsDo,  PortID(PortNumber))
import Network.Socket (socketToHandle, setSocketOption, SocketOption(ReuseAddr), Socket, PortNumber)
import System.IO
import Control.Concurrent (forkIO, ThreadId)
import Control.Monad (forever)
import Control.Monad.Fix (fix)
import Data.Word(Word16)

data HttpRequest = HttpRequest { url :: String,
                                 contentType :: String,
                                 version :: (Int, Int),
                                 cookies :: [(String, String)],
                                 userAgent :: String,
                                 keepAlive :: Bool
                               }

data HttpResponse = HttpResponse { code :: Int,
                                   body :: String
                                 }

main :: IO ()
main = runServer $ onPort 8080

onPort :: Int -> PortID
onPort p = PortNumber $ fromIntegral p

runServer :: PortID -> IO ()
runServer port = withSocketsDo $ do
    sock <- listenOn port
    setSocketOption sock ReuseAddr 1
    forever $ acceptConnection connectionHandler sock
    where
        connectionHandler = handleHttpConnection dummyHandler

acceptConnection :: (Handle -> IO()) -> Socket -> IO ThreadId
acceptConnection handler socket = do
        connection <- accept socket
        let (handle, host, port) = connection
        forkIO $ handler $ handle

handleHttpConnection :: (HttpRequest -> IO HttpResponse) -> Handle -> IO ()
handleHttpConnection handler connection = parseHttpRequest connection >>= handler >>= writeHttpResponse connection

writeHttpResponse :: Handle -> HttpResponse -> IO()
writeHttpResponse handle response = do
    hPutStr handle "HTTP/1.1 200 OK\n"
    hPutStr handle "Content-Length: 11\n\n"
    hPutStr handle "Hello World\n"
    hFlush handle
    hClose handle -- TODO: Keep this open if it's a keepalive request

parseHttpRequest :: Handle -> IO HttpRequest
parseHttpRequest handle = do
    line <- hGetLine handle
    return HttpRequest{url="/foo", contentType="text/html", version=(1,1), cookies=[], userAgent="Mozilla", keepAlive=False}

dummyHandler :: HttpRequest -> IO HttpResponse
dummyHandler request = return HttpResponse{code=200, body="Hello world!"}
