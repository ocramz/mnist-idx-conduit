{-# options_ghc -Wno-unused-imports #-}
module Main where


-- conduit
import Conduit (MonadResource, runResourceT, (.|), runConduitRes)
import qualified Data.Conduit as C (ConduitT, runConduit, bracketP, yield)
import qualified Data.Conduit.Combinators as C (sourceFile, head, chunksOfExactlyE, chunksOfE, map, mapM, head, print)

import Data.IDX.Conduit -- (sourceIDX0, decodeHeader)

main :: IO ()
main = do
  -- decodeHeader "assets/mnist/train-images-idx3-ubyte"
  tt0 "assets/mnist/train-images-idx3-ubyte"
  -- case mm of
  --   Just i -> print i
  --   Nothing -> pure ()


tt0 fp = runConduitRes $
  sourceIdxSparse fp .|
  C.print
