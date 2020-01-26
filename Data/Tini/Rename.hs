{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Tini.Rename (rename) where
import Foreign.C (CInt (..))
import Foreign.C.String (withCString, CString)

-- | Rename @src@ to @dst@, overwriting @dst@ if it already exists.
rename :: FilePath -> FilePath -> IO ()
rename src dst = withCString src $ \src' -> do
  result <- withCString dst (c_rename src')
  if result /= 0 then error "unable to rename file" else return ()

foreign import ccall "rename" c_rename :: CString -> CString -> IO CInt