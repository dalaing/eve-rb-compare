module Main where

import Control.Monad (forever)
import System.IO (hSetBuffering, hSetEcho, stdin, stdout, BufferMode(..))

import Control.Event.Handler (AddHandler, newAddHandler)

import Reactive.Banana
import Reactive.Banana.Frameworks

-- An event source is a bridge between
-- - an AddHandler, which allows us to register for events inside of an event network
-- - a fire method, which allows us to cause events to trigger from outside of an event network
data EventSource a =
  EventSource {
    addHandler :: AddHandler a
  , fire :: a -> IO ()
  }

mkEventSource :: IO (EventSource a)
mkEventSource =
  uncurry EventSource <$> newAddHandler

-- We pack up our sources of events into a data structure, because it usually turns out to be convenient later on.
data Sources =
  Sources {
    sKeyChar :: EventSource Char
  }

mkSources :: IO Sources
mkSources =
  Sources <$> mkEventSource

-- Our event loop fires these events over and over
eventLoop :: Sources -> IO ()
eventLoop (Sources sc) =
    forever loop
  where
    loop = do
      c <- getChar
      fire sc c

-- Our network description tells us what to do with these things
networkDescription :: Sources -> MomentIO ()
networkDescription (Sources sc) = do
  -- We register for updates with `fromAddHandler`, giving us an event that fires whenever the user presses a key
  eChar <- fromAddHandler $ addHandler sc

  -- We carry out `IO` with `reactimate`
  reactimate $ (\c -> putStrLn $ "You pressed " ++ [c]) <$> eChar

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False

  -- This is the usual plumbing
  sources <- mkSources
  network <- compile $ networkDescription sources
  actuate network
  eventLoop sources

