module Primes where

import           System.Random
import           Data.Binary
import qualified Data.ByteString.Lazy.Internal as BS

primeCandidate :: IO Int
primeCandidate = do
  g <- newStdGen
  return $ decode $ foldl combineEncode
                          (BS.Empty)
                          (take 32 $ randomRs (0, 4294967295) g)

combineEncode :: BS.ByteString -> Int -> BS.ByteString
combineEncode bs x = bs <> (encode x)