{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid
import Data.Foldable (mapM_)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Control.Foldl as L
import Control.Foldl.Transduce
import Control.Foldl.Transduce.Text
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Control.Exception

import System.IO

toHandle :: Handle -> FoldM IO B.ByteString ()
toHandle h = FoldM (\_ -> B.hPut h) (pure ()) (\_ -> pure ())

testGroupIO :: IO String
testGroupIO = fmap show $  
    withFile "/dev/null" AppendMode $ \h1 ->
        withFile "/dev/null" AppendMode $ \h2 ->
            flip L.foldM (repeat "aabbccddeeffgghhiijj") $
                foldsM (chunksOf 7) (L.generalize L.list) (L.handlesM traverse (toHandle h1)) *>
                foldsM (chunksOf 3) (L.generalize L.list) (L.handlesM traverse (toHandle h2))

testPureGroup :: IO String
testPureGroup = evaluate . show $
    flip L.fold (cycle [1,-1]) $
        (+) <$> groups (chunksOf 3) (evenly (transduce (surround [0::Int] [0,0]))) L.sum <*>
                groups (chunksOf 7) (evenly (transduce (surround [0,0] [0]))) L.sum

testQuiesce :: IO String
testQuiesce = evaluate . show $ 
    flip L.fold (cycle [1,-1]) $ L.simplify $ quiesce foldThatFails 
    where
        foldThatFails = FoldM (\_ _-> throwE ()) (return ()) (\_ -> return (0::Int))

testQuiesceWith :: IO String
testQuiesceWith = fmap show $ 
    flip L.foldM (cycle [1,-1]) $
        quiesceWith (L.generalize L.sum) foldThatFails
    where 
        foldThatFails = FoldM (\_ _-> throwE ()) (return ()) (\_ -> return (0::Int))

concurrenTests :: [IO String]
concurrenTests = [testGroupIO,testPureGroup,testQuiesce,testQuiesceWith]

main :: IO ()
main = do
    rs <- runConcurrently (mapM Concurrently concurrenTests)
    mapM_ putStrLn rs
