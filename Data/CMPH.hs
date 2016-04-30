{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}


module Data.CMPH ( CMPH, hash, buildHash, size ) where
import           Control.Exception      (bracket)
import           Control.Monad          (guard, liftM)
import           Data.Array
import           Data.Array.IO          (IOArray, newArray, newArray_,
                                         writeArray)
import           Data.Array.Storable    (StorableArray, withStorableArray)
import           Data.Array.Unsafe      (unsafeFreeze)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as S
import qualified Data.ByteString.Unsafe as Unsafe
import           Data.Monoid
import qualified Data.Set               as Set
import           Data.Word
import           Foreign                (Ptr)
import           Foreign.C.String       (CString)
import           Foreign.C.Types
import           Foreign.Marshal.Array  ()
import           GHC.Arr                (unsafeAt)
import           Prelude                hiding (lookup)
import           System.IO.Unsafe       (unsafePerformIO)

data ForeignHash

foreign import ccall unsafe "stub.h cmph_search" c_cmph_search
  :: Ptr ForeignHash -> CString -> CUInt -> IO CULong
foreign import ccall unsafe "stub.h build_hash"  c_build_hash
  :: Ptr CString -> CUInt -> IO (Ptr ForeignHash)
foreign import ccall unsafe "stub.h free_ptrs"   c_free_ptrs
  :: CUInt -> Ptr CString -> IO ()
foreign import ccall unsafe "string.h strdup" c_strdup
  :: CString -> IO CString

data CMPH = CMPH { rawHash :: Ptr ForeignHash
                 , size    :: Word32
                 }

-- we pull a few dumb tricks to get around some partiality in libcmph.
--
--   no empty strings allowed
--     so prepend 'a' to all strings
--   can't hash an empty list
--     so add an element to empty lists.
--   no repeats
--     strip them out before passing to the c lib
--   can't have embedded nulls in the inserted strings
--     return Nothing. Too hard.

buildHash :: [BS.ByteString] -> IO (Maybe CMPH)
buildHash input'
  | all noNull input' = do
      let input = uniqued ("":"b":input')
      let len = fromIntegral $ length input
      raw <-
        bracket (buildCPtrs (map ("a"<>) input))
                (freeCPtrs len)
                (\a -> withStorableArray a (`c_build_hash` (CUInt len)))
      return $ Just $ CMPH raw len
  | otherwise = return Nothing

uniqued :: Ord a => [a] -> [a]
uniqued = Set.toList . Set.fromList

noNull :: BS.ByteString -> Bool
noNull = (==Nothing) . S.find (=='\NUL')

hash :: CMPH-> BS.ByteString -> IO Word64
hash cmph bs = do
  (CULong w) <- S.useAsCStringLen ("a"<>bs) $ \(cstr,len) -> c_cmph_search (rawHash cmph) cstr (CUInt $ fromIntegral len)
  return w

freeCPtrs :: Word32
          -> StorableArray Int CString
          -> IO ()
freeCPtrs len arr = withStorableArray arr $ c_free_ptrs (CUInt len)

buildCPtrs bs = do
  buffer <- newArray_ (0,length bs-1)
  (`mapM_` (zip [0..] bs)) $ \(i,bs) ->
    S.useAsCStringLen bs $ \(cstr,len) -> do
      writeArray buffer i =<< c_strdup cstr
  return buffer
