import Data.Monoid
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Control.Foldl as L
import Control.Foldl.Transduce
import Control.Foldl.Transduce.Text
import Control.Concurrent.Async

import System.IO

toHandle :: Handle -> FoldM IO B.ByteString ()
toHandle h = FoldM (\_ -> B.hPut h) (pure ()) (\_ -> pure ())

main :: IO ()
main = do
   return ()
