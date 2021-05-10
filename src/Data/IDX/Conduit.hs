{-# LANGUAGE DeriveFoldable #-}
{-# options_ghc -Wno-unused-imports #-}
{-|

Streaming decoders for the IDX format used in the MNIST dataset

The data items are passed down the conduit either in dense or sparse form
-}
module Data.IDX.Conduit (
  -- * Dense
  sourceIdx,
  -- * Sparse
  sourceIdxSparse,
  Sparse,
  sBufSize, sNzComponents
                        )where


import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (Foldable(..))
import Data.Int (Int8, Int16, Int32)
import Data.Word (Word8)
import GHC.IO.Handle (Handle, hSeek, SeekMode(..), hClose)
import System.IO (IOMode(..), openBinaryFile)
-- binary
import Data.Binary (Binary(..), Get, getWord8, putWord8, decode, decodeOrFail)
import Data.Binary.Get (runGetOrFail)
-- bytestring
import qualified Data.ByteString.Lazy as LBS (ByteString, hGet, readFile, toStrict, map)
import qualified Data.ByteString.Lazy.Internal as LBS (unpackBytes)
-- conduit
import Conduit (MonadResource, runResourceT, (.|), runConduitRes)
import qualified Data.Conduit as C (ConduitT, runConduit, bracketP, yield)
-- containers
import Data.Sequence (Seq, (|>))
-- vector
import qualified Data.Vector as V (Vector, replicateM, length, forM_, head, tail)
import qualified Data.Vector.Unboxed as VU (Unbox, Vector, fromList)


-- | Outputs dense data buffers in the 0-255 range
--
-- In the case of MNIST dataset, 0 corresponds to the background of the image.
sourceIdx :: MonadResource m =>
             FilePath -- ^ filepath of uncompressed IDX data file
          -> C.ConduitT a (VU.Vector Word8) m ()
sourceIdx = sourceIDX_ (\ _ bs -> VU.fromList $ components bs)


-- | Outputs sparse data buffers (i.e without zero components)
--
-- This incurs at least one additional data copy of each vector, but 
sourceIdxSparse :: MonadResource m =>
                   FilePath -- ^ filepath of uncompressed IDX data file
                -> C.ConduitT a (Sparse Word8) m ()
sourceIdxSparse = sourceIDX_ (\n bs -> Sparse n (sparsify $ components bs))


-- sourceIdxLabels = sourceIDX_ (\_ bs -> )

sourceIDX_ :: MonadResource m =>
              (Int -> LBS.ByteString -> o)
           -> FilePath -- ^ filepath of uncompressed IDX data file
           -> C.ConduitT i o m r
sourceIDX_ buildf fp = withReadHdl fp $ \handle -> do
  hlbs <- liftIO $ LBS.hGet handle 4
  case decodeOrFail hlbs of
    Left (_, _, e) -> error e
    Right (_, _, IDXMagic _ ndims) -> do
      let
        bytesDimsVec = 4 * ndims -- each dim is a 32 bit (4 byte) int
      dvlbs <- liftIO $ LBS.hGet handle bytesDimsVec
      case getDims ndims dvlbs of
        Left e -> error e
        Right vv -> do
          let
            -- ndata = V.head vv
            bufsize = product $ V.tail vv
            go h = do
              b <- liftIO $ LBS.hGet h bufsize
              liftIO $ hSeek h RelativeSeek (fromIntegral bufsize)
              C.yield $ buildf bufsize b
              go h
          go handle

sparsify :: (Foldable t) => t Word8 -> VU.Vector (Int, Word8)
sparsify xs = VU.fromList $ toList $ snd $ foldl ins (0, mempty) xs
  where
    ins (i, acc) x =
      let x' = fromEnum x
      in if x' /= 0
      then (succ i, acc |> (i, x))
      else (succ i, acc)

components :: LBS.ByteString -> [Word8]
components = LBS.unpackBytes

-- | Sparse buffer (containing only nonzero entries)
data Sparse a = Sparse {
  sBufSize :: !Int -- ^ total number of entries in the _dense_ buffer, i.e. including the zeros
  , sNzComponents :: VU.Vector (Int, a) -- ^ nonzero components, together with the linear index into their dense counterpart
  }  deriving (Eq, Show)

getDims :: Num a =>
           Int -- ^ number of dimensions
        -> LBS.ByteString -> Either String (V.Vector a)
getDims n lbs = case runGetOrFail gg lbs of
  Left (_, _, e) -> Left e
  Right (_, _, x) -> Right x
  where
    gg = V.replicateM n (fromIntegral <$> getInt32)

withReadHdl :: MonadResource m =>
               FilePath
            -> (Handle -> C.ConduitT i o m r) -- ^ read from the handle
            -> C.ConduitT i o m r
withReadHdl fp = C.bracketP (openBinaryFile fp ReadMode) hClose

data IDXHeader = IDXHeader {
    idxMagic :: IDXMagic
  , idxDimensions :: V.Vector Int
  , ixdNumItems :: Int
                           } deriving (Show)

-- | "magic number" starting the file header for the IDX format
--
-- as per http://yann.lecun.com/exdb/mnist/
--
-- 4 bytes header ("magic number")
data IDXMagic = IDXMagic {
  idxType :: IDXContentType
  , idxNumDims :: Int
                         } deriving (Show)

instance Binary IDXMagic where
  get = do
    -- first 2 bytes are 0
    _ <- getWord8 >> getWord8
    -- third byte encodes the type of data
    ty <- get :: Get IDXContentType
    -- fourth byte encode the number of dimensions
    nDims <- fromIntegral <$> getWord8
    pure $ IDXMagic ty nDims
  put d = do
    putWord8 0
    putWord8 0
    -- Third byte is content type
    put $ idxType d
    -- Fourth byte is number of dimensions
    put $ (fromIntegral (idxNumDims d) :: Word8)




-- | A type to describe the content, according to IDX spec
data IDXContentType =
   IDXUnsignedByte
   | IDXSignedByte
   | IDXShort
   | IDXInt
   | IDXFloat
   | IDXDouble
   deriving Show

instance Binary IDXContentType where
    get = do
      w <- getWord8
      case w of
        0x08 -> return IDXUnsignedByte
        0x09 -> return IDXSignedByte
        0x0B -> return IDXShort
        0x0C -> return IDXInt
        0x0D -> return IDXFloat
        0x0E -> return IDXDouble
        _ -> fail $ "Unrecognized IDX content type: " ++ (show w)

    put IDXUnsignedByte = putWord8 0x08
    put IDXSignedByte   = putWord8 0x09
    put IDXShort        = putWord8 0x0B
    put IDXInt          = putWord8 0x0C
    put IDXFloat        = putWord8 0x0D
    put IDXDouble       = putWord8 0x0E

-- Data.Binary uses big-endian format
-- getInt8 :: Get Int8
-- getInt8 = get

-- getInt16 :: Get Int16
-- getInt16 = get

getInt32 :: Get Int32
getInt32 = get

-- getFloat :: Get Float
-- getFloat = get

-- getDouble :: Get Double
-- getDouble = get
