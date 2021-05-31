{-# language CPP #-}
module Pure.Auth (module Export) where

#ifdef __GHCJS__
import Pure.Auth.GHCJS as Export
import Pure.Auth.API as Export
#else
import Pure.Auth.GHC as Export
#endif
