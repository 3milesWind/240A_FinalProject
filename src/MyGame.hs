{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module MyGame
  ( initGame2
  , Game2(..)
  , MyDirection(..)
  , myheight, mywidth
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Extra (orM)
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen)

-- Types

type Coord = V2 Int

type Snake = Seq Coord

type Rock = Seq Coord

data Game2 = Game2
  { _d :: MyDirection       -- ^ direction
  , _player :: Coord         -- ^ the location of the player will be modified via I/O
  } deriving (Show)


data Stream a = a :| Stream a
  deriving (Show)

data MyDirection
  = MyNorth
  | MySouth
  | MyEast
  | MyWest
  deriving (Eq, Show)

makeLenses ''Game2
-- Constants

myheight, mywidth :: Int
myheight = 20
mywidth = 20

-- Functions

-- | Step forward in time

initGame2 :: IO Game2
initGame2 = do
  let x = 0
      y = 0
      g = Game2
        {
          _d = MySouth
        , _player = (V2 x y)
        }
  return (execState initState g)

initState :: State Game2 ()
initState = do
  s <- get
  put s


fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")
