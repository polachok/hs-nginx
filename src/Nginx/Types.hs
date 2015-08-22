{-# LANGUAGE DataKinds, FlexibleInstances #-}
module Nginx.Types(Context(..),
             Switch(..),
             Size(..),
             ValueOrOff,
             Serializable, serialize) where

import Data.Text (Text)

data Context = MainContext | EventsContext | HttpContext | ServerContext | LocationContext | IfContext
    deriving (Show)

data Switch = On | Off
    deriving (Show)

data Size = Size Integer Char
    deriving (Show)

data ValueOrOff a = Value a | Off_ 
    deriving (Show)

class Show a => Serializable a where
    serialize :: a -> Text
