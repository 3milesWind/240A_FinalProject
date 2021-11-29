{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module MyGame
  ( initGame2
  ,initGame3
  , moves
  , decrease_step
  , check_die
  , Game2(..)
  , MyDirection(..)
  , myheight, mywidth
  , player, d, gameOver, stepsRemain, princess, win, rock, monster, unwalkable
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
import System.Random (Random(..), newStdGen, getStdRandom)
import System.IO.Unsafe

-- Types

type Coord = V2 Int
--type Rock = Seq Coord
--type Rock = Seq Coord

data Game2 = Game2
  { _d      :: MyDirection       -- ^ direction
  , _player :: Coord             -- ^ the location of the player will be modified via I/O
  , _gameOver :: Bool            -- ^ the bool value mark the game is live or dead
  , _stepsRemain :: Int          -- ^ track the number of stepsRemain
  , _princess :: Coord
  , _win :: Bool
  , _rock :: [Coord]
  , _monster :: [Coord]
  , _unwalkable :: [Coord]
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
myheight = 6
mywidth = 7

-- Functions


outrange2 :: [Coord]
outrange2 = [(V2 2 2), (V2 3 2), (V2 4 2), (V2 5 2), (V2 6 2)
           ,(V2 0 3), (V2 5 3), (V2 6 3)
           ,(V2 0 4), (V2 6 4), (V2 6 1), (V2 6 5)
           ,(V2 0 5), (V2 1 5), (V2 2 5), (V2 3 5)
           ]

rockLocation2 :: [Coord]
rockLocation2 = [ (V2 1 0), (V2 3 0)
               , (V2 1 1), (V2 4 1)
               ]

monsterLocation2 :: [Coord]
monsterLocation2 = [ (V2 2 3), (V2 4 3)
                   , (V2 3 4)           
                   ]
-- | Step forward in time

initGame2 :: IO Game2
initGame2 = do
  let x = 5
      y = 5
      g = Game2
        {
          _d = MySouth
        , _player = (V2 x y)
        , _gameOver = False
        , _stepsRemain = 100
        , _princess = (V2 (mywidth-1) 0)
        , _win = False
        , _unwalkable = outrange2
        , _rock = rockLocation2  
        , _monster = monsterLocation2
        }
  return (execState initState g)


initGame3 :: IO Game2
initGame3 = do
  let x = 0
      y = 0
      g = Game2
        {
          _d = MySouth
        , _player = (V2 x y)
        , _gameOver = False
        , _stepsRemain = 100
        , _princess = (V2 (mywidth-1) (myheight-1))
        , _win = False
        , _unwalkable = outrange2
        , _rock = rockLocation2
        , _monster = monsterLocation2
        }
  return (execState initState g)

initState :: State Game2 ()
initState = do
  s <- get
  put s

moves :: MyDirection -> Game2 -> Game2
moves MyNorth g = do
  let (V2 x y) = g ^. player
  if g ^. win == True then g
  else if g ^. gameOver == True then g
  else if y >= myheight - 1 then g
  else if (V2 x (y+1)) `elem` (g ^. unwalkable) then g
  else if (rockExists g MyNorth) && (movable g MyNorth) == False then g
  else if (rockExists g MyNorth) && (movable g MyNorth) then 
    check_win(check_die(decrease_step(moveRock g MyNorth))) 
  else if (monsterExists g MyNorth) then
    check_win(check_die(decrease_step(killMonster g MyNorth)))
  else 
    check_win ((check_die (decrease_step g)) & player %~ (\(V2 a b) -> (V2 a (b+1))))

moves MyEast g = do
  let (V2 x y) = g ^. player
  if g ^. win == True then g
  else if g ^. gameOver == True then g
  else if x >= mywidth - 1 then g
  else if (V2 (x+1) y) `elem` (g ^. unwalkable) then g
  else if (rockExists g MyEast) && (movable g MyEast) == False then g
  else if (rockExists g MyEast) && (movable g MyEast) then 
    check_win(check_die(decrease_step(moveRock g MyEast))) 
  else if (monsterExists g MyEast) then
    check_win(check_die(decrease_step(killMonster g MyEast)))
  else 
    check_win ((check_die (decrease_step g)) & player %~ (\(V2 a b) -> (V2 (a+1) b)))

moves MyWest g = do
  let (V2 x y) = g ^. player
  if g ^. win == True then g
  else if g ^. gameOver == True then g
  else if x <= 0 then g
  else if (V2 (x-1) y) `elem` (g ^. unwalkable) then g
  else if (rockExists g MyWest) && (movable g MyWest) == False then g
  else if (rockExists g MyWest) && (movable g MyWest) then 
    check_win(check_die(decrease_step(moveRock g MyWest))) 
  else if (monsterExists g MyWest) then
    check_win(check_die(decrease_step(killMonster g MyWest)))
  else 
    check_win ((check_die (decrease_step g)) & player %~ (\(V2 a b) -> (V2 (a-1) b)))

moves MySouth g = do
  let (V2 x y) = g ^. player
  if g ^. win == True then g
  else if g ^. gameOver == True then g
  else if y <= 0 then g
  else if (V2 x (y-1)) `elem` (g ^. unwalkable) then g
  else if (rockExists g MySouth) && (movable g MySouth) == False then g
  else if (rockExists g MySouth) && (movable g MySouth) then 
    check_win(check_die(decrease_step(moveRock g MySouth))) 
  else if (monsterExists g MySouth) then
    check_win(check_die(decrease_step(killMonster g MySouth)))
  else 
    check_win ((check_die (decrease_step g)) & player %~ (\(V2 a b) -> (V2 a (b-1))))

moves _ g = g

decrease_step :: Game2 -> Game2
decrease_step g =  g & stepsRemain %~ (\n -> (n-1))

check_die :: Game2 -> Game2
check_die g = do
  if g ^. stepsRemain == 0 then g & gameOver %~ (\_ -> True)
  else g

check_win :: Game2 -> Game2
check_win g = do
  if g ^. player == g ^. princess then g & win %~ (\_ -> True)
  else g

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")

--check if rock exists in the next step
rockExists :: Game2 -> MyDirection -> Bool
rockExists g MyNorth = do
  let (V2 x y) = g ^. player
  if (V2 x (y+1)) `elem` (g ^. rock) then True
  else False

rockExists g MySouth = do
  let (V2 x y) = g ^. player
  if (V2 x (y-1)) `elem` (g ^. rock) then True
  else False 

rockExists g MyEast = do
  let (V2 x y) = g ^. player
  if (V2 (x+1) y) `elem` (g ^. rock) then True
  else False

rockExists g MyWest = do
  let (V2 x y) = g ^. player
  if (V2 (x-1) y) `elem` (g ^. rock) then True
  else False

monsterExists :: Game2 -> MyDirection -> Bool
monsterExists g MyNorth = do
   let (V2 x y) = g ^. player
   if (V2 x (y+1)) `elem` (g ^. monster) then True
   else False

monsterExists g MySouth = do
  let (V2 x y) = g ^. player
  if (V2 x (y-1)) `elem` (g ^. monster) then True
  else False 

monsterExists g MyEast = do
  let (V2 x y) = g ^. player
  if (V2 (x+1) y) `elem` (g ^. monster) then True
  else False

monsterExists g MyWest = do
  let (V2 x y) = g ^. player
  if (V2 (x-1) y) `elem` (g ^. monster) then True
  else False

killMonster :: Game2 -> MyDirection -> Game2
killMonster g MyNorth = do
  let (V2 x y) = g ^. player
  let curr_monster = (V2 x (y+1))
  g & monster %~ (\list -> (delete curr_monster list))

killMonster g MySouth = do
  let (V2 x y) = g ^. player
  let curr_monster = (V2 x (y-1))
  g & monster %~ (\list -> (delete curr_monster list))

killMonster g MyEast = do
  let (V2 x y) = g ^. player
  let curr_monster = (V2 (x+1) y)
  g & monster %~ (\list -> (delete curr_monster list))

killMonster g MyWest = do
  let (V2 x y) = g ^. player
  let curr_monster = (V2 (x-1) y)
  g & monster %~ (\list -> (delete curr_monster list))

--check if this rock is movable
movable :: Game2 -> MyDirection -> Bool
movable g MyNorth = do
  let (V2 x y) = g ^. player
  if (y+2) >= myheight then False
  else if (V2 x (y+2)) `elem` (g ^. rock) then False
  else if (V2 x (y+2)) `elem` (g ^. monster) then False
  else if (V2 x (y+2)) `elem` (g ^. unwalkable) then False
  else if (V2 x (y+2)) == (g ^. princess) then False 
  else True

movable g MySouth = do
  let (V2 x y) = g ^. player
  if (y-2) < 0 then False
  else if (V2 x (y-2)) `elem` (g ^. rock) then False
  else if (V2 x (y-2)) `elem` (g ^. monster) then False
  else if (V2 x (y-2)) `elem` (g ^. unwalkable) then False
  else if (V2 x (y-2)) == (g ^. princess) then False 
  else True

movable g MyWest = do
  let (V2 x y) = g ^. player
  if (x-2) < 0 then False
  else if (V2 (x-2) y) `elem` (g ^. rock) then False
  else if (V2 (x-2) y) `elem` (g ^. monster) then False
  else if (V2 (x-2) y) `elem` (g ^. unwalkable) then False
  else if (V2 (x-2) y) == (g ^. princess) then False 
  else True

movable g MyEast = do
  let (V2 x y) = g ^. player
  if (x+2) >= mywidth then False
  else if (V2 (x+2) y) `elem` (g ^. rock) then False
  else if (V2 (x+2) y) `elem` (g ^. monster) then False
  else if (V2 (x+2) y) `elem` (g ^. unwalkable) then False
  else if (V2 (x+2) y) == (g ^. princess) then False 
  else True

--move
moveRock :: Game2 -> MyDirection -> Game2
moveRock g MyNorth = do
  let (V2 x y) = g ^. player
  let curr_rock = (V2 x (y+1))
  -- delete curr_rock from the list, then insert new rock location into the list
  g & rock %~ (\list -> (V2 x (y+2)) : (delete curr_rock list))

moveRock g MySouth = do
  let (V2 x y) = g ^. player
  let curr_rock = (V2 x (y-1))
  -- delete curr_rock from the list, then insert new rock location into the list
  g & rock %~ (\list -> (V2 x (y-2)) : (delete curr_rock list))

moveRock g MyEast = do
  let (V2 x y) = g ^. player
  let curr_rock = (V2 (x+1) y)
  -- delete curr_rock from the list, then insert new rock location into the list
  g & rock %~ (\list -> (V2 (x+2) y) : (delete curr_rock list))

moveRock g MyWest = do
  let (V2 x y) = g ^. player
  let curr_rock = (V2 (x-1) y)
  -- delete curr_rock from the list, then insert new rock location into the list
  g & rock %~ (\list -> (V2 (x-2) y) : (delete curr_rock list))

--delete an element from the list
delete :: Eq a => a -> [a] -> [a]
delete deleted list = [x | x <- list, x /= deleted]
