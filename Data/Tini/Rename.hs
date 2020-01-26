{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Tini.Rename (rename) where
import Foreign.C (CInt (..))
import Foreign.C.String (withCString, CString)

foreign import ccall "rename"
  c_rename :: CString -> CString -> IO CInt

-- | Rename @src@ to @dst@, overwriting @dst@ if it already exists.
rename :: FilePath -> FilePath -> IO ()
rename src dst = withCString src $ \src' -> do
  withCString dst $ \dst' -> do
    result <- c_rename src' dst'
    if result /= 0
      then error "unable to rename file"
      else return ()