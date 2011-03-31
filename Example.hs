{-# LANGUAGE OverloadedStrings #-}
module Example where

import Network.HandlerSocket
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Instances
import Debug.Trace (trace)

run' :: Connected -> IO ()
run' c = find_all c                               >>= (\a ->
           trace(show a ++ "\nadd") add c         >>
           find_all c                             >>= (\b ->
           trace(show b ++ "\nupdate") update' c  >>
           find_all c                             >>= (\d ->
           trace(show d ++ "\ndelete") delete c   >>
           find_all c))) >>= print
  where
    find_all c = get (Query Gte ["0"] 10 0) c
    add      c = insert [ "4", "fourth" ] c
    update'  c = update (Query Eq ["4"] 1 0) [ "4", "modified!" ] c
    delete   c = remove (Query Eq ["4"] 1 0) c

main = do
  hs <- connect'
  c  <- open' hs
  either (putStrLn . show) (run') c
  disconnect hs
  where
    connect'   = connect "127.0.0.1" 9999
    open' hs   = open (Index 1 "test" "foo" "PRIMARY" [ "id", "data" ]) hs

-- vim: set ts=2 sw=2 et :
