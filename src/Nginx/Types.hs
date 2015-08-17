{-# LANGUAGE DataKinds, FlexibleInstances #-}
module Nginx.Types(Context(..),
             Switch(..),
             Size(..),
             ValueOrOff) where

data Context = MainContext | EventsContext | HttpContext | ServerContext | LocationContext | IfContext
    deriving (Show)

data Switch = On | Off
    deriving (Show)

data Size = Size Integer Char
    deriving (Show)

data ValueOrOff a = Value a | Off_ 
    deriving (Show)
