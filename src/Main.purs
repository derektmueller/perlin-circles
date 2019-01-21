module Main where

import Prelude 
import Control.Monad.Reader.Trans
import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(..), maybe)
import Control.Safely
import Data.Tuple
import Data.Array
import Data.Maybe
import Data.Unfoldable
import Data.Int (toNumber, floor)
import Web.HTML (window)
import Web.HTML.Window (innerWidth, innerHeight)

import P5 
import P5.Rendering
import P5.Color
import P5.Shape
import P5.Math
import P5.Structure
import P5.Transform

type AppState = {
  p5 :: P5
}

initialState :: Maybe AppState
initialState = Nothing

enumFromThenTo :: Int -> Int -> Int -> Array Int
enumFromThenTo a b c = do
  unfoldr (go (b - a) c) a
  where
    go step to e
      | e <= to = Just (Tuple e (e + step))
      | otherwise = Nothing

type Config = {
  noiseStep :: Number
}

drawPoint :: P5 
  -> Tuple (Tuple Number Int) (Tuple Number Int) 
  -> ReaderT Config Effect Unit
drawPoint p (Tuple (Tuple x xi) (Tuple y yi)) = do
  config <- ask
  let noiseStep = config.noiseStep
      xNoise = noiseStep * (toNumber xi)
      yNoise = noiseStep * (toNumber yi)
      transScale = 1.3
      radScale = 8.0
  noiseFactor <- 
    lift $ noise p xNoise (Just yNoise) Nothing
  lift $ push p
  lift $ translate2 
    p (x * noiseFactor * transScale) 
    (y * noiseFactor * transScale) (Just $ -y)
  lift $ ellipse 
    p 0.0 0.0 (radScale * noiseFactor)
    (Just (radScale * noiseFactor))
  lift $ pop p
  pure unit

main :: Maybe AppState -> Effect (Maybe AppState)
main mAppState = do
  win <- window
  w <- toNumber <$> innerWidth win
  h <- toNumber <$> innerHeight win
  p <- maybe getP5 (\x -> pure x.p5) mAppState

  setup p do
    _ <- createCanvas p w h (Just CREATE_CANVAS_RENDERER_WEBGL)
    noLoop p
    pure unit

  draw p do
    background4 p 150.0 Nothing
    stroke4 p 0.0 (Just 25.0)
    fill4 p 255.0 (Just 200.0)

    let screenFraction = 5.0
        xRadius = w / screenFraction
        yRadius = h / screenFraction
        step = 1.3
        config = {
          noiseStep: 0.02
        }
        xs = toNumber <$> enumFromThenTo 
          (floor (-xRadius))
          (floor (-xRadius + step))
          (floor xRadius)
        ys = toNumber <$> enumFromThenTo 
          (floor (-yRadius))
          (floor (-yRadius + step))
          (floor yRadius)
        points = do
          Tuple x xi <- zip xs (0..(length xs))
          Tuple y yi <- zip ys (0..(length ys))
          pure $ Tuple 
            (Tuple x xi) (Tuple y yi)

    runReaderT 
      (traverse_ (drawPoint p) points) config

    pure unit

  case mAppState of
    (Just _) -> do
      clear p
      redraw p Nothing
    _ -> pure unit

  pure $ Just { p5: p }
