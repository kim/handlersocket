{-# LANGUAGE OverloadedStrings #-}
module Network.HandlerSocket
  ( HandlerSocket
  , Index(..)
  , Connected
  , Query(..)
  , Failure
  , Result
  , Ok
  , Comp(..)
  , IndexId
  , Value
  , Row
  , Limit
  , Offset
  , connect
  , disconnect
  , open
  , get
  , update
  , insert
  , remove ) where

import           Control.Monad
import           Control.Monad.Instances
import           Data.Bits (shift, xor)
import qualified Data.ByteString.Lazy as B
import           Data.ByteString.Lazy (ByteString)
import           Data.ByteString.Lazy.Char8 (pack, unpack)
import           Data.Char (digitToInt, ord)
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator as E
import           Data.Enumerator (($$))
import           Data.Ix (inRange)
import qualified Data.List as L
import           Data.Word (Word8)
import qualified Network
import           System.IO


data Comp = Eq | Gt | Lt | Gte | Lte
instance Show Comp where
  show Eq  = "="
  show Gt  = ">"
  show Lt  = "<"
  show Gte = ">="
  show Lte = "<="

type IndexId = Int
type Value   = ByteString
type Row     = [Value]
type Limit   = Int
type Offset  = Int

newtype HandlerSocket = HandlerSocket Handle

data Index = Index { xid  :: IndexId
                   , db   :: ByteString
                   , tbl  :: ByteString
                   , idx  :: ByteString
                   , cols :: [ByteString] }
            deriving (Show)

data Connected = Connected { i  :: Index
                           , hs :: HandlerSocket }

data Query = Query Comp [Value] Limit Offset

data Failure = Failure { errno :: Int, errmsg :: ByteString } deriving (Show)
data Result  = Result { rows :: [Row] } deriving (Show)
data Ok      = Ok deriving (Show)

connect :: Network.HostName -> Network.PortNumber -> IO HandlerSocket
connect host port = do
  handle <- Network.connectTo host (Network.PortNumber port)
  hSetBinaryMode handle True
  return (HandlerSocket handle)

disconnect :: HandlerSocket -> IO ()
disconnect (HandlerSocket h) = hClose h

open :: Index -> HandlerSocket -> IO (Either Failure Connected)
open index hs = run (Connected index hs) (Open index) >>= (\r -> return $ open' r)
  where
    open' (Response 0 _ _) = Right (Connected index hs)
    open' resp             = Left  (err resp)

get :: Query -> Connected -> IO (Either Failure Result)
get q c = run c (Get q) >>= (\r -> return $ get' r)
  where
    get' (Response 0 _ rows) = Right (Result rows)
    get' resp                = Left (err resp)

update :: Query -> [Value] -> Connected -> IO (Either Failure Ok)
update q vals c = run c (Update q vals) >>= (\r -> return $ update' r)
  where
    update' (Response 0 _ _) = Right Ok
    update' resp             = Left (err resp)

insert :: [Value] -> Connected -> IO (Either Failure Ok)
insert vals c = run c (Insert vals) >>= (\r -> return $ insert' r)
  where
    insert' (Response 0 _ _) = Right Ok
    insert' resp             = Left (err resp)

remove :: Query -> Connected -> IO (Either Failure Ok)
remove q c = run c (Remove q) >>= (\r -> return $ remove' r)
  where
    remove' (Response 0 _ _) = Right Ok
    remove' resp             = Left (err resp)


-- private
data Request = Get Query
             | Update Query [Value]
             | Insert [Value]
             | Remove Query
             | Open Index

data Response = Response { rcode :: Int
                         , num   :: Int
                         , rrows :: [[ByteString]] }
              deriving (Show)


run :: Connected -> Request -> IO Response
run (Connected i hs) req = send hs request >> recv hs >>= (\r ->
  (return $ parseResponse r))
  where
    request = serialize (xid i) req
    send (HandlerSocket h) b = B.hPut h b >> hFlush h
    recv (HandlerSocket h)   = E.run_ (EB.enumHandle 1024 h $$ EB.takeWhile (/=eof))
    eof = 0x0a

serialize :: IndexId -> Request -> ByteString
serialize i (Get q)       = encode $ serialize'' i : serialize' q
serialize i (Update q vs) = encode $ [ serialize'' i ] ++ serialize' q ++ [ "U" ] ++ vs
serialize i (Insert vs)   = encode $ serialize'' i : "+" : serialize'' (length vs) : vs
serialize i (Remove q)    = encode $ [ serialize'' i ] ++ serialize' q ++ [ "D" ]
serialize i (Open i')     = encode $ [ "P"
                                     , serialize'' i
                                     , db i'
                                     , tbl i'
                                     , idx i'
                                     , B.concat $ L.intersperse "," (cols i') ]

serialize' :: Query -> [ByteString]
serialize' (Query comp vals limit offset) =
  [ pack $ show comp
  , pack $ show $ length vals ]
  ++ vals ++
  [ pack $ show limit
  , pack $ show offset ]

serialize'' :: Int -> ByteString
serialize'' = pack . show

parseResponse :: ByteString -> Response
parseResponse response = Response errc numc rows'
  where
    decd  = dec response
    xs    = B.split 0x09 decd
    errc  = digitToInt $ head $ unpack $ head xs
    numc  = digitToInt $ head $ unpack $ xs !! 1
    rows' = rows'' $ drop 2 xs
    rows'' []  = []
    rows'' xs' = take numc xs' : (rows'' $ drop numc xs')


encode :: [ByteString] -> ByteString
encode bs = B.concatMap enc (B.intercalate "\t" bs) `B.snoc` 0x0a

enc :: Word8 -> ByteString
enc c = if is_sep || is_latin1 then pass
        else if is_ctrl then ctrl else nul
  where
    is_sep     = 0x09 == c
    is_latin1  = inRange(0x10, 0xff) c
    is_ctrl    = inRange(0x00, 0x0f) c
    pass = B.pack [ c ]
    nul  = B.pack [ 0 :: Word8 ]
    ctrl = B.pack [ 0x01 :: Word8, c `shift` 0x40 ]


dec :: ByteString -> ByteString
dec s = if B.null t
        then s
        else
          h `B.snoc` (B.head t `xor` 0x40) `B.append` dec (B.tail t)
  where
    (h,t) = B.break (==0x01) s


err :: Response -> Failure
err resp = Failure errno (msg $ rrows resp)
  where
    errno = rcode resp
    msg [] = ""
    msg rs = (head . head) rs


-- vim: set ts=2 sw=2 et :
