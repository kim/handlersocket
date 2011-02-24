module Example where

import HandlerSocket
import Debug.Trace (trace)

type Connected = (Server, Int)

openIndex :: Server -> IO Result
openIndex s = open s 8 "test" "foo" "PRIMARY" ["id", "data"]

find :: Server -> Int -> IO Result
find s i = get s i Gte ["0"] 10 0

add :: Server -> Int -> IO Result
add s i = insert s i ["4", "fourth"]

delete :: Server -> Int -> IO Result
delete s i = remove s i Eq ["4"] 1 0

modify :: Server -> Int -> IO Result
modify s i = update s i Eq ["4"] ["4", "modified!"] 1 0

runAndShow :: Server -> Int -> (Server -> Int -> IO Result) -> IO ()
runAndShow s i fn = fn s i >>= (putStrLn . show)

main = do
  s <- connect "127.0.0.1" 9999
  r <- openIndex s
  case r of ErrorResult _ _ ->
              putStrLn $ show r
            OpenResult i ->
              mapM_ (runAndShow s i)
                    [ find
                    , add
                    , find
                    , modify
                    , find
                    , delete
                    , find ]
  disconnect s

-- vim: set ts=2 sw=2 et :
