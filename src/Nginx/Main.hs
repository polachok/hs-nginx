{-# LANGUAGE OverloadedStrings, GADTs, StandaloneDeriving, DataKinds, FlexibleInstances, KindSignatures #-}
module Nginx.Main(main) where

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Attoparsec.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import Text.Parser.Combinators (endBy)

import Nginx.Types (Context(..))
import Nginx.Events (EventsDirective,events)
import Nginx.Utils
import Nginx.Http.Core (http,HttpDirective)

data MainDirective (a :: Context) where
    UserDirective :: Text -> MainDirective MainContext
    WorkerProcessesDirective :: Integer -> MainDirective MainContext
    TimerResolutionDirective :: Text -> MainDirective MainContext
    PidDirective :: Text -> MainDirective MainContext
    EventsDirective :: [EventsDirective 'EventsContext] -> MainDirective MainContext
    HttpDirective :: [HttpDirective 'HttpContext] -> MainDirective MainContext

deriving instance Show (MainDirective a)

main = mainDirective `sepBy` (skipComment <|> skipSpace)
mainDirective = user <|> workerProcesses <|> pid <|> eventsD <|> timerResolution <|> httpD

user = UserDirective <$> textValueDirective "user"
workerProcesses = WorkerProcessesDirective <$> intValueDirective "worker_processes"
pid = PidDirective <$> textValueDirective "pid"
eventsD = EventsDirective <$> (string "events" *> skipSpace *> char '{' *> skipSpace *> many events <* skipSpace <* char '}')
timerResolution = TimerResolutionDirective <$> textValueDirective "timer_resolution"
httpD = HttpDirective <$> (string "http" *> skipSpace *> char '{' *> skipSpace *> http <* skipSpace <* char '}')
