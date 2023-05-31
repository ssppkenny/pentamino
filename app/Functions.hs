{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-identities #-}
{-# LANGUAGE OverloadedStrings #-}

module Functions
  ( State
  , board
  , initialState
  ) where

import Data.Array as A (Array, Ix, (!), (//), array, bounds)
import Data.Map as M
  ( Map
  , (!)
  , empty
  , fromList
  , insert
  , intersection
  , mapWithKey
  )
import qualified Data.Set as Set
import Miso.String (MisoString, toMisoString)

import System.Random

newtype Elements =
  Elements
    { coords :: [(Integer, Integer)]
    }
  deriving (Show, Eq)

data Figure =
  Figure
    { number :: Integer
    , coord :: (Integer, Integer)
    , elements :: Elements
    , placed :: Bool
    }
  deriving (Show, Eq)

type Board = Array (Integer, Integer) Integer

data State =
  State
    { figures :: [Figure]
    , current :: Maybe Figure
    , board :: Board
    }
  deriving (Show, Eq)

initialState =
  State
    { figures = initialFigures
    , current = Nothing
    , board = figuresToBoard initialFigures initialBoard
    }
  where
    initialFigures = allFigures

figureToBoard :: Figure -> Board -> Board
figureToBoard fig board =
  case fig of
    Figure n (x, y) (Elements elements) placed ->
      if placed
        then board // (map (\(a, b) -> ((a + x, b + y), n)) elements)
        else board

figuresToBoard :: [Figure] -> Board -> Board
figuresToBoard figs board =
  case figs of
    [] -> board
    (x:xs) -> figuresToBoard xs (figureToBoard x board)

allFigures :: [Figure]
allFigures =
  [ Figure 1 (1, 1) (Elements [(0, 0), (0, 1), (0, 2), (0, 3), (0, 4)]) False
  , Figure 2 (1, 1) (Elements [(0, 0), (0, 1), (1, 1), (2, 1), (1, 2)]) False
  , Figure 3 (1, 1) (Elements [(0, 0), (0, 1), (0, 2), (0, 3), (1, 0)]) False
  , Figure 4 (1, 1) (Elements [(0, 0), (0, 1), (1, 1), (1, 2), (1, 3)]) False
  , Figure 5 (1, 1) (Elements [(0, 0), (0, 1), (0, 2), (1, 1), (1, 2)]) False
  , Figure 6 (1, 1) (Elements [(0, 1), (0, 2), (0, 3), (1, 1), (2, 1)]) False
  ]

integerToMisoString :: Integer -> MisoString
integerToMisoString n = toMisoString (fromIntegral n :: Int)

initialBoard :: Array (Integer, Integer) Integer
initialBoard =
  array ((1, 1), (6, 10)) [((i, j), 0) | i <- [1 .. 6], j <- [1 .. 10]]
