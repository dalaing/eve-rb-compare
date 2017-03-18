module Main where

import Control.Monad (forever)
import Control.Concurrent (threadDelay, forkIO)
import System.IO (hSetBuffering, hSetEcho, stdin, stdout, BufferMode(..))
import System.Random (randomRIO)

import Control.Event.Handler (AddHandler, newAddHandler)

import Reactive.Banana
import Reactive.Banana.Frameworks

data EventSource a =
  EventSource {
    addHandler :: AddHandler a
  , fire :: a -> IO ()
  }

mkEventSource :: IO (EventSource a)
mkEventSource = uncurry EventSource <$> newAddHandler

leftmost :: [Event a] -> Event a
leftmost = foldl (unionWith const) never

data Sources =
  Sources {
    sKeyChar :: EventSource Char
  , sTick    :: EventSource ()
  }

mkSources :: IO Sources
mkSources =
  Sources <$>  mkEventSource <*> mkEventSource

leftKey :: Event Char -> Event ()
leftKey e = () <$ filterE (== 'a') e

rightKey :: Event Char -> Event ()
rightKey e = () <$ filterE (== 'd') e

pos :: MonadMoment m => Event () -> Event () -> m (Event Int)
pos eLeft eRight =
  accumE 0 . unions $ [
      pred <$ eLeft
    , succ <$ eRight
    ]

treasure :: Event () -> Event Int -> MomentIO (Behavior [Int])
treasure eTick ePos = do
  eRandom <- execute $ liftIO (randomRIO (1, 20)) <$ eTick
  bTreasures <- accumB ([] :: [Int]) . unions $ [
      (:) <$> eRandom
    , (filter . (/=)) <$> ePos
    ]
  return bTreasures

-- We create a collected event that fires whenever the position event fires at a position which contains treasure
collected :: Behavior [Int] -> Event Int -> Event ()
collected bTreasure ePos =
  () <$ filterApply ((\t -> (`elem` t)) <$> bTreasure) ePos

-- We accumulate collection events to create a behavior for the score
score :: MonadMoment m => Event () -> m (Behavior Int)
score eCollected =
  accumB 0 (succ <$ eCollected)

tunnel :: String
tunnel = replicate 20 '.'

replaceAt :: Char -> Int -> String -> String
replaceAt _ _ [] = []
replaceAt c 0 (_:xs) = c:xs
replaceAt c n (x:xs) = x:replaceAt c (n-1) xs

toString :: Int -> [Int] -> Int -> String
toString n t s =
    let
      addTreasures t tunnel = foldr (replaceAt '%') tunnel t
      gameMap = replaceAt '$' n . addTreasures t $ tunnel
    in
      gameMap ++ " " ++ show s

render :: String -> IO ()
render s = do
  putChar '\r'
  putStr s

networkDescription :: Sources -> MomentIO ()
networkDescription (Sources sc st) = do
  -- * Handle the position stuff
  eChar <- fromAddHandler $ addHandler sc
  let
    eLeft = leftKey eChar
    eRight = rightKey eChar
  ePos <- pos eLeft eRight
  bPos <- stepper 0 ePos

  -- * Handle the treasure stuff
  eTick <- fromAddHandler $ addHandler st
  bTreasure <- treasure eTick ePos

  -- * Handle the score stuff
  let eCollected = collected bTreasure ePos
  bScore <- score eCollected

  let bRender = toString <$> bPos <*> bTreasure <*> bScore

  eC <- changes bRender
  reactimate' $ fmap (fmap render) eC

eventLoop :: Sources -> IO ()
eventLoop (Sources sc st) = do
    forkIO . forever $ do
      threadDelay 3000000
      fire st ()
    forever loop
  where
    loop = do
      c <- getChar
      fire sc c

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False

  sources <- mkSources
  network <- compile $ networkDescription sources
  actuate network
  eventLoop sources

