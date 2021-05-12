{-# options_ghc -Wno-unused-imports #-}
module Data.IDX.ConduitSpec where

import Control.Monad (when, replicateM, replicateM_)
import Data.Foldable (Foldable(..), traverse_, for_)
import Data.Word (Word8)

-- conduit
import Conduit (MonadResource, runResourceT, (.|), runConduitRes)
import qualified Data.Conduit as C (ConduitT, runConduit, bracketP, yield)
import qualified Data.Conduit.Combinators as C (print, sinkList, sinkFile, map, takeExactly)
import qualified Data.Conduit.List as C (sourceList)
-- hspec
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
-- vector
import qualified Data.Vector as V (Vector, replicateM, length, forM_, head, tail)
import qualified Data.Vector.Unboxed as VU (Unbox, Vector, length, fromList, toList, foldl, (!), replicate)

import Data.IDX.Conduit (sourceIdx, sinkIdx, readHeader)

spec :: Spec
spec = do
  describe "Data.IDX.ConduitSpec" $ do
    it "should roundtrip" $ do
      writeData dats
      ll <- readData
      ll `shouldBe` dats

testfp :: FilePath
testfp = "test/ConduitSpec_temp.idx" 

readData :: IO [VU.Vector Word8]
readData = runConduitRes $
             sourceIdx testfp .|
             C.sinkList

writeData :: [VU.Vector Word8] -> IO ()
writeData ds = runConduitRes $
                 C.sourceList ds .|
                 sinkIdx testfp 11 [5, 2]

dats :: [VU.Vector Word8]
dats = map vv [0 .. 10]

-- src :: Monad m => C.ConduitT i (VU.Vector Word8) m ()
-- src = for_ [0 .. 10] $ \ i -> C.yield (vv i)

vv :: Int -> VU.Vector Word8
vv i = VU.replicate 10 (fromIntegral i)
