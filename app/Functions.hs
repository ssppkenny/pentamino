{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-identities #-}
{-# LANGUAGE OverloadedStrings #-}

module Functions
  ( State
  , board
  , initialState
  ) where

import Data.Array as A (Array, Ix, (!), (//), array)
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
    { coords :: [(Int, Int)]
    }
  deriving (Show, Eq)

data Figure =
  Figure
    { number :: Int
    , coord :: (Int, Int)
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

initialState = State {figures = [], current = Nothing, board = initialBoard}

colors :: M.Map Integer MisoString
colors =
  M.fromList
    [ (0, "white")
    , (1, "#DDBB99")
    , (2, "#EEAAAA")
    , (3, "#CCCC88")
    , (4, "#AAEEAA")
    , (5, "#BBDD99")
    , (6, "#99DDBB")
    , (7, "#88CCCC")
    , (8, "#99BBDD")
    , (9, "#AAAAEE")
    , (10, "#BB99DD")
    , (11, "#CC88CC")
    , (12, "#DD99BB")
    ]

integerToMisoString :: Integer -> MisoString
integerToMisoString n = toMisoString (fromIntegral n :: Int)

initialBoard :: Array (Integer, Integer) Integer
initialBoard =
  array ((1, 1), (6, 10)) [((i, j), 0) | i <- [1 .. 6], j <- [1 .. 10]]
