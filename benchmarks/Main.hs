import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Criterion.Main
import System.Posix.IO
import System.Posix.Types
import Control.DeepSeq

server :: IO (MVar (), MVar ())
server = do
  mv1 <- newEmptyMVar
  mv2 <- newEmptyMVar
  forkIO $ forever $ do
    takeMVar mv1
    putMVar mv2 ()
  return (mv1, mv2)

instance NFData Fd where
  rnf (Fd x) = x `seq` ()

main :: IO ()
main = defaultMain
    [ env server $ \ ~(mv1, mv2) ->
        bench "sync" $ nfIO $ do
          putMVar mv1 ()
          takeMVar mv2
    , env createPipe $ \ ~(p1, p2) ->
        bench "pipe" $ nfIO $ do
          fdWrite p2 "a"
          threadWaitRead p1
          fdRead p1 1
    ]
