{-# options_ghc -Wno-unused-imports #-}
module Data.IDX.Conduit where

-- binary
import Data.Binary (Binary(..), Get)
-- binary-conduit
import Data.Conduit.Serialization.Binary (conduitGet)
-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString as LBS (ByteString)
-- minst-idx
import Data.IDX (IDXData, IDXLabels, decodeIDX, decodeIDXLabels, idxDimensions, isIDXReal)

-- decodeC bs = case decodeIDX bs of
--   Just idx -> 

