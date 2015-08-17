{-# LANGUAGE OverloadedStrings, GADTs, StandaloneDeriving, DataKinds, FlexibleInstances, KindSignatures #-}
module Nginx.Events(EventsDirective,events) where

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Attoparsec.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import Text.Parser.Combinators (endBy)

import Nginx.Types (Context(..))
import Nginx.Utils

data EventsDirective (a :: Context) where
    WorkerConnectionsDirective :: Integer -> EventsDirective EventsContext

deriving instance Show (EventsDirective a)

events = workerConnections
workerConnections = WorkerConnectionsDirective <$> intValueDirective "worker_connections"
