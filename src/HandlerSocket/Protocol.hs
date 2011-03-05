module HandlerSocket.Protocol
  ( Server
  , connect, disconnect
  ) where

import           HandlerSocket.Types

import           Data.Char
import           Data.Bits
import           Data.Binary
import           Data.Binary.Put
import           Data.Binary.Get
import qualified Data.ByteString.Lazy as B
import           Data.ByteString.Lazy (ByteString)
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Ix
import qualified Data.List as L
import           Control.Applicative ((<$>))
import qualified Network
import qualified Network.Socket.ByteString.Lazy as S
import           System.IO
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator as E
import           Data.Enumerator (($$))
import           Control.Monad.IO.Class (liftIO)


data Msg = Get Int Op [String] Int Int
         | Update Int Op [String] [String] Int Int
         | Insert Int [String]
         | Remove Int Op [String] Int Int
         | Open Int String String String [String]

data Response = Response { rcode :: Int
                         , num   :: Int
                         , rows  :: [[ByteString]] }
                  deriving (Show)

putMsg :: Msg -> Put
putMsg (Get index op cols limit offset) =
  putSs $ [ show index, show op, show $ length cols ]
          ++ cols
          ++ [ show limit, show offset ]
putMsg (Update index op cols vals limit offset) =
  putSs $ [ show index, show op, show $ length cols ]
          ++ cols
          ++ [ show limit, show offset, "U" ]
          ++ vals
putMsg (Insert index vals) =
  putSs $ [ show index, "+", show $ length vals ] ++ vals
putMsg (Remove index op cols limit offset) =
  putSs $ [ show index, show op, show $ length cols ]
          ++ cols
          ++ [ show limit, show offset, "D" ]
putMsg (Open id db table index cols) =
  putSs $ [ "P", show id, db, table, index, (L.intercalate "," cols) ]

parseResponse :: ByteString -> Response
parseResponse response = Response errc numc rows
  where
    decd = dec response
    xs   = B.split 0x09 decd
    errc = digitToInt $ head $ unpack (head xs)
    numc = digitToInt $ head $ unpack (xs !! 1)
    rows = rows' $ drop 2 xs
    rows' []  = []
    rows' xs' = take numc xs' : (rows' $ drop numc xs')

putSs :: [String] -> Put
putSs ss = do
  mapM_ enc $ L.intercalate "\t" ss
  put '\n'

enc :: Char -> Put
enc c = if sep || latin1 then putLit
        else if ctrl then putCtrl else putNul
  where
    i = ord c
    w = fromIntegral i :: Word8
    sep     = '\t' == c
    latin1  = inRange(0x10, 0xff) i
    ctrl    = inRange(0x00, 0x0f) i
    putLit  = putWord8 w
    putNul  = putWord8 (0 :: Word8)
    putCtrl = do
                putWord8 (0x01 :: Word8)
                putWord8 $ fromIntegral $ i `shift` 0x40

dec :: ByteString -> ByteString
dec s = if B.null t
        then s
        else
          h `B.snoc` (B.head t `xor` 0x40) `B.append` dec (B.tail t)
  where
    (h,t) = B.break (==0x01) s

-- IO
newtype Server = Server Handle

connect :: Network.HostName -> Network.PortNumber -> IO Server
connect host port = do
  handle <- Network.connectTo host (Network.PortNumber port)
  hSetBinaryMode handle True
  return (Server handle)

disconnect :: Server -> IO ()
disconnect (Server h) = hClose h

send :: Server -> Msg -> IO ()
send (Server handle) msg = do
  let b = runPut $ putMsg msg
  B.hPut handle b >> hFlush handle

recv :: Server -> IO ByteString
recv (Server handle) = E.run_ (EB.enumHandle 1024 handle $$ EB.takeWhile (/=eof))
  where
    eof = (fromIntegral . ord) '\n'

sendrecv :: Server -> Msg -> IO Response
sendrecv server msg = do
  send server msg
  b <- recv server
  return (parseResponse b)

-- util
err :: Response -> Result
err resp = ErrorResult errno (msg $ rows resp)
  where
    errno = rcode resp
    msg [] = ""
    msg rs = (unpack . head . head) rs

-- HandlerSocket interface
instance HandlerSocket Server where
  open server id db table index cols = do
    r <- sendrecv server (Open id db table index cols)
    if rcode r == 0
      then return $ OpenResult id
      else return $ err r

  get server index (Query op vals limit offset) = do
    r <- sendrecv server (Get index op vals limit offset)
    if rcode r == 0
      then return $ GetResult (rows' r)
      else return $ err r
    where
      rows' rs = [ [ unpack xs | xs <- xxs ] | xxs <- rows rs ]

  update server index (Query op qvals limit offset) vals = do
    r <- sendrecv server (Update index op qvals vals limit offset)
    if rcode r == 0
      then return Ok
      else return (err r)

  insert server index vals = do
    r <- sendrecv server (Insert index vals)
    if rcode r == 0
      then return Ok
      else return (err r)

  remove server index (Query op vals limit offset) = do
    r <- sendrecv server (Remove index op vals limit offset)
    if rcode r == 0
      then return Ok
      else return (err r)

-- vim: set ts=2 sw=2 et :
