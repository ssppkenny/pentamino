-- | Haskell language pragma
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-identities #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

-- | Haskell module declaration
module Main where

import Styles

import System.Random (getStdGen)

import Data.Array as A ((!), (//), bounds)

-- | Miso framework import
import Miso

import Functions
import Language.Javascript.JSaddle.Warp as JSaddle

runApp :: JSM () -> IO ()
runApp = JSaddle.run 8080

-- | Type synonym for an application model
type Model = State

-- | Sum type for application events
data Action =
  NoOp
  deriving (Show, Eq)

-- | Entry point for a miso application
main :: IO ()
main =
  runApp $ do
    startApp
      App
        { initialAction = NoOp -- initial action to be executed on application load
        , model = initialState
        , update = updateModel -- update function
        , view = viewModel -- view function
        , events = defaultEvents -- default delegated events
        , subs = [] -- empty subscription list
        , mountPoint = Nothing -- mount point for application (Nothing defaults to 'body')
        , logLevel = Off
        }

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m =
  div_
    [style_ containerStyle]
    (map
       (\i ->
          div_
            [style_ rowStyle]
            (map (\j -> div_ [style_ (cellStyle (b A.! (i, j)))] []) [f2 .. t2]))
       [f1 .. t1])
  where
    b = board m
    bs = bounds b
    p1 = fst bs
    p2 = snd bs
    f1 = fst p1
    f2 = snd p1
    t1 = fst p2
    t2 = snd p2
