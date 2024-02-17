{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_HaskellLambda (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "HaskellLambda"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Haskell interpreter for lambda calculus"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
