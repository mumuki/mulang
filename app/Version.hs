{-# LANGUAGE CPP #-}

module Version (prettyVersion, version, compilationTimestamp) where

prettyVersion :: String
prettyVersion = "Mulang Release " ++ version ++ ", compiled on " ++ compilationTimestamp

version :: String
version = "4.1.0"

compilationTimestamp :: String
compilationTimestamp = __DATE__ ++ " " ++ __TIME__
