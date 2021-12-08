{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module MyGame
  ( initGame1
  , initGame2
  , initGame3
  , initGame4
  , initGame5
  , initGame6
  , moves
  , decrease_step
  , check_die
  , Game2(..)
  , MyDirection(..)
  , myheight, mywidth, level3_height, level3_width
  , player, d, gameOver, stepsRemain, princess, win, rock, monster, unwalkable, level, trap
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
  , _level :: Int
  , _trap :: [Coord]
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

level3_height, level3_width :: Int
level3_height = 4
level3_width = 9
--fixed setup for each difficulty
--level 1
outrange1 :: [Coord]
outrange1 = [ (V2 0 0), (V2 1 0), (V2 2 0), (V2 3 0)
            , (V2 2 1), (V2 3 1)
            , (V2 2 2), (V2 3 2)
            , (V2 2 3), (V2 3 3)
            , (V2 2 4), (V2 0 4), (V2 0 5), (V2 5 5)
            , (V2 6 5)
            ]


rockLocation1 :: [Coord]
rockLocation1 = [ (V2 4 3), (V2 5 3), (V2 6 3)
                ]

monsterLocation1 :: [Coord]
monsterLocation1 = [ (V2 5 1), (V2 6 0)
                   , (V2 1 4)
                   ]

trapLocation1 :: [Coord]
trapLocation1 = [ (V2 1 3)
                , (V2 3 4)
                , (V2 4 4), (V2 4 3)
                , (V2 5 3), (V2 5 2)
                ]
--leve 2
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

--leve 6
outrange6 :: [Coord]
outrange6 = [(V2 0 0), (V2 0 4), (V2 0 5), (V2 1 0), (V2 1 4), (V2 1 5),
              (V2 2 4), (V2 2 5), (V2 3 0), (V2 3 5), (V2 4 0),
           (V2 4 2), (V2 5 0), (V2 6 0), (V2 6 1), (V2 6 2), (V2 6 5), (V2 6 3)
           ]

rockLocation6 :: [Coord]
rockLocation6 = [ (V2 1 1), (V2 1 2)
               , (V2 1 3), (V2 3 2), (V2 4 3), (V2 5 3)
               ]

monsterLocation6 :: [Coord]
monsterLocation6 = [ (V2 4 1), (V2 5 5)
                     
                   ]
trapLocation6:: [Coord]
trapLocation6 = [(V2 1 2), (V2 1 3)]

--level 3 
outrange3 :: [Coord]
outrange3 = [ (V2 0 0), (V2 0 3)
            , (V2 2 3)
            , (V2 6 3)
            , (V2 8 0), (V2 8 3)
            ]

rockLocation3 :: [Coord]
rockLocation3 = [ (V2 0 2)
                , (V2 1 1), (V2 1 3)
                , (V2 2 1)
                , (V2 3 0), (V2 3 1), (V2 3 2), (V2 3 3)
                , (V2 4 2)
                , (V2 5 2)
                , (V2 6 0), (V2 6 1)
                , (V2 7 1)
                ]
        
monsterLocation3 :: [Coord]
monsterLocation3 = []
-- | Step forward in time

outrange4 :: [Coord]
outrange4 = [(V2 0 5), (V2 1 5), (V2 2 5), (V2 3 5), (V2 4 5), (V2 5 5), (V2 6 5)
           ,(V2 0 4), (V2 1 4), (V2 2 4), (V2 3 4), (V2 4 4), (V2 5 4), (V2 6 4)
           ,(V2 0 0), (V2 1 0), (V2 2 0), (V2 3 0), (V2 4 0), (V2 5 0), (V2 6 0)
           ]

rockLocation4 :: [Coord]
rockLocation4 = [ (V2 0 3), (V2 1 3), (V2 2 3), (V2 3 3), (V2 4 3), (V2 5 3), (V2 6 3)
               , (V2 0 1), (V2 1 1), (V2 2 1), (V2 3 1), (V2 4 1),(V2 5 1),(V2 6 1)
               ]

monsterLocation4 :: [Coord]
monsterLocation4 = [ (V2 1 2), (V2 2 2), (V2 3 2), (V2 4 2), (V2 5 2)]

outrange5 :: [Coord]
outrange5 = [(V2 0 5), (V2 1 5), (V2 2 5), (V2 3 5), (V2 4 5), (V2 5 5), (V2 6 5)
           ,(V2 0 4), (V2 2 4),  (V2 4 4), (V2 5 4), (V2 6 4)
           ,(V2 0 0), (V2 1 0),  (V2 3 0), (V2 4 0), (V2 5 0), (V2 6 0)
           ,(V2 6 3),(V2 6 1)
           ]

monsterLocation5 :: [Coord]
monsterLocation5 = [ (V2 0 3), (V2 1 3), (V2 2 3), (V2 3 3), (V2 4 3), (V2 5 3)
               , (V2 0 1), (V2 1 1), (V2 2 1), (V2 3 1), (V2 4 1),(V2 5 1)
               ]

rockLocation5 :: [Coord]
rockLocation5 = [ (V2 1 2), (V2 2 2), (V2 3 2), (V2 4 2), (V2 5 2)]

trapLocation5:: [Coord]
trapLocation5 = [(V2 5 3)]

initGame1 :: IO Game2
initGame1 = do
  let g = Game2
        {
          _d = MySouth
        , _player = (V2 0 2)
        , _gameOver = False
        , _stepsRemain = 100
        , _princess = (V2 4 0)
        , _win = False
        , _unwalkable = outrange1
        , _rock = rockLocation1  
        , _monster = monsterLocation1
        , _level = 1
        , _trap = trapLocation1
        }
  return (execState initState g)

initGame2 :: IO Game2
initGame2 = do
  let
      g = Game2
        {
          _d = MySouth
        , _player = (V2 5 5)
        , _gameOver = False
        , _stepsRemain = 24
        , _princess = (V2 6 0)
        , _win = False
        , _unwalkable = outrange2
        , _rock = rockLocation2
        , _monster = monsterLocation2
        , _level = 2
        , _trap = []
        }
  return (execState initState g)

initGame3 :: IO Game2
initGame3 = do
  let
    g = Game2
      {
        _d = MySouth
      , _player = (V2 1 0)
      , _gameOver = False
      , _stepsRemain = 17
      , _princess = (V2 8 2)
      , _win = False
      , _unwalkable = outrange3
      , _rock = rockLocation3
      , _monster = monsterLocation3
      , _level = 3
      , _trap = []
      }
  return (execState initState g)

initGame4 :: IO Game2
initGame4 = do
  let
    g = Game2
      {
        _d = MySouth
      , _player = (V2 0 2)
      , _gameOver = False
      , _stepsRemain = 1000
      , _princess = (V2 6 2)
      , _win = False
      , _unwalkable = outrange4
      , _rock = rockLocation4
      , _monster = monsterLocation4
      , _level = 4
      , _trap = []
      }
  return (execState initState g)
  
initGame5 :: IO Game2
initGame5 = do
  let
    g = Game2
      {
        _d = MySouth
      , _player = (V2 0 2)
      , _gameOver = False
      , _stepsRemain =1000
      , _princess = (V2 6 2)
      , _win = False
      , _unwalkable = outrange5
      , _rock = rockLocation5
      , _monster = monsterLocation5
      , _level = 5
      , _trap = trapLocation5
      }
  return (execState initState g)

initGame6 :: IO Game2
initGame6 = do
  let
      g = Game2
        {
          _d = MySouth
        , _player = (V2 0 1)
        , _gameOver = False
        , _stepsRemain = 29
        , _princess = (V2 6 4)
        , _win = False
        , _unwalkable = outrange6
        , _rock = rockLocation6
        , _monster = monsterLocation6
        , _level = 6
        , _trap = trapLocation6
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
  else if (g ^. level /= 3) && y >= myheight - 1 then g
  else if (g ^. level == 3) && y >= level3_height then g
  else if (V2 x (y+1)) `elem` (g ^. unwalkable) then g
  else if (rockExists g MyNorth) && (movable g MyNorth) == False then g
  else if (rockExists g MyNorth) && (movable g MyNorth) then 
    check_win(check_die(moveObject(decrease_step g MyNorth) MyNorth)) 
  else if (monsterExists g MyNorth) && (movable g MyNorth) then
    check_win(check_die(moveObject(decrease_step g MyNorth) MyNorth)) 
  else if (monsterExists g MyNorth) && ((movable g MyNorth) == False) then
    check_win(check_die(decrease_step(killMonster g MyNorth) MyNorth))
  else 
    check_win ((check_die (decrease_step g MyNorth)) & player %~ (\(V2 a b) -> (V2 a (b+1))))

moves MyEast g = do
  let (V2 x y) = g ^. player
  if g ^. win == True then g
  else if g ^. gameOver == True then g
  else if (g ^. level /= 3) && x >= mywidth - 1 then g
  else if (g ^. level == 3) && x >= level3_width - 1 then g
  else if (V2 (x+1) y) `elem` (g ^. unwalkable) then g
  else if (rockExists g MyEast) && (movable g MyEast) == False then g
  else if (rockExists g MyEast) && (movable g MyEast) then 
    check_win(check_die(moveObject(decrease_step g MyEast) MyEast)) 
  else if (monsterExists g MyEast) && (movable g MyEast) then
    check_win(check_die(moveObject(decrease_step g MyEast) MyEast)) 
  else if (monsterExists g MyEast) && ((movable g MyEast) == False) then
    check_win(check_die(decrease_step(killMonster g MyEast) MyEast))
  else 
    check_win ((check_die (decrease_step g MyEast)) & player %~ (\(V2 a b) -> (V2 (a+1) b)))

moves MyWest g = do
  let (V2 x y) = g ^. player
  if g ^. win == True then g
  else if g ^. gameOver == True then g
  else if x <= 0 then g
  else if (V2 (x-1) y) `elem` (g ^. unwalkable) then g
  else if (rockExists g MyWest) && (movable g MyWest) == False then g
  else if (rockExists g MyWest) && (movable g MyWest) then 
    check_win(check_die(moveObject(decrease_step g MyWest) MyWest)) 
  else if (monsterExists g MyWest) && (movable g MyWest) then
    check_win(check_die(moveObject(decrease_step g MyWest) MyWest)) 
  else if (monsterExists g MyWest) && ((movable g MyWest) == False) then
    check_win(check_die(decrease_step(killMonster g MyWest) MyWest))
  else 
    check_win ((check_die (decrease_step g MyWest)) & player %~ (\(V2 a b) -> (V2 (a-1) b)))

moves MySouth g = do
  let (V2 x y) = g ^. player
  if g ^. win == True then g
  else if g ^. gameOver == True then g
  else if y <= 0 then g
  else if (V2 x (y-1)) `elem` (g ^. unwalkable) then g
  else if (rockExists g MySouth) && (movable g MySouth) == False then g
  else if (rockExists g MySouth) && (movable g MySouth) then 
    check_win(check_die(moveObject(decrease_step g MySouth) MySouth)) 
  else if (monsterExists g MySouth) && (movable g MySouth) then
    check_win(check_die(moveObject(decrease_step g MySouth) MySouth)) 
  else if (monsterExists g MySouth) && ((movable g MySouth) == False) then
    check_win(check_die(decrease_step(killMonster g MySouth) MySouth))
  else 
    check_win ((check_die (decrease_step g MySouth)) & player %~ (\(V2 a b) -> (V2 a (b-1))))

moves _ g = g




decrease_step :: Game2 -> MyDirection -> Game2
decrease_step g dir = do
    let (V2 x y) = g ^. player
    if dir == MySouth && (V2 x (y-1)) `elem` g ^. trap && 
      ((V2 x (y-1)) `elem` g ^. rock) == False && ((V2 x (y-1)) `elem` g ^. monster) == False
        then g & stepsRemain %~ (\n -> (n-2))
    else if dir == MyNorth && (V2 x (y+1)) `elem` g ^. trap && 
      ((V2 x (y+1)) `elem` g ^. rock) == False && ((V2 x (y+1)) `elem` g ^. monster) == False
        then g & stepsRemain %~ (\n -> (n-2))
    else if dir == MyWest && (V2 (x-1) y) `elem` g ^. trap && 
      ((V2 (x-1) y) `elem` g ^. rock) == False && ((V2 (x-1) y) `elem` g ^. monster) == False
        then g & stepsRemain %~ (\n -> (n-2))
    else if dir == MyEast && (V2 (x+1) y) `elem` g ^. trap && 
      ((V2 (x+1) y) `elem` g ^. rock) == False && ((V2 (x+1) y) `elem` g ^. monster) == False
        then g & stepsRemain %~ (\n -> (n-2))
    else
      g & stepsRemain %~ (\n -> (n-1))

check_die :: Game2 -> Game2
check_die g = do
  if g ^. stepsRemain <= 0 then g & gameOver %~ (\_ -> True)
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

--check if this object is movable
movable :: Game2 -> MyDirection -> Bool
movable g MyNorth = do
  let (V2 x y) = g ^. player
  if (g ^. level /=3) && ((y+2) >= myheight) then False
  else if ( g ^. level == 3) && ((y+2) >= level3_height) then False
  else if (V2 x (y+2)) `elem` (g ^. rock) then False
  else if (V2 x (y+2)) `elem` (g ^. monster) then False
  else if (V2 x (y+2)) `elem` (g ^. unwalkable) then False
  else if (V2 x (y+2)) == (g ^. princess) then False 
  else if (V2 x (y+2)) `elem` (g ^. trap) then False
  else True

movable g MySouth = do
  let (V2 x y) = g ^. player
  if (g ^. level /=3) && ((y-2) < 0) then False
  else if ( g ^. level == 3) && ((y-2) < 0) then False
  else if (V2 x (y-2)) `elem` (g ^. rock) then False
  else if (V2 x (y-2)) `elem` (g ^. monster) then False
  else if (V2 x (y-2)) `elem` (g ^. unwalkable) then False
  else if (V2 x (y-2)) == (g ^. princess) then False
  else if (V2 x (y-2)) `elem` (g ^. trap) then False
  else True

movable g MyWest = do
  let (V2 x y) = g ^. player
  if (g ^. level /=3) && ((x-2) < 0) then False
  else if ( g ^. level == 3) && ((x-2) < 0) then False
  else if (V2 (x-2) y) `elem` (g ^. rock) then False
  else if (V2 (x-2) y) `elem` (g ^. monster) then False
  else if (V2 (x-2) y) `elem` (g ^. unwalkable) then False
  else if (V2 (x-2) y) == (g ^. princess) then False 
  else if (V2 (x-2) y) `elem` (g ^. trap) then False
  else True

movable g MyEast = do
  let (V2 x y) = g ^. player
  if (g ^. level /=3) && ((x+2) >= mywidth) then False
  else if ( g ^. level == 3) && ((x+2) >= level3_width) then False
  else if (V2 (x+2) y) `elem` (g ^. rock) then False
  else if (V2 (x+2) y) `elem` (g ^. monster) then False
  else if (V2 (x+2) y) `elem` (g ^. unwalkable) then False
  else if (V2 (x+2) y) == (g ^. princess) then False 
  else if (V2 (x+2) y) `elem` (g ^. trap) then False
  else True

--move
moveObject :: Game2 -> MyDirection -> Game2
moveObject g MyNorth = do
  let (V2 x y) = g ^. player
  let object = (V2 x (y+1))
  -- delete curr_rock from the list, then insert new rock location into the list
  if object `elem` (g ^. rock) then
    g & rock %~ (\list -> (V2 x (y+2)) : (delete object list))
  else g & monster %~ (\list -> (V2 x (y+2)) : (delete object list))

moveObject g MySouth = do
  let (V2 x y) = g ^. player
  let object = (V2 x (y-1))
  -- delete curr_rock from the list, then insert new rock location into the list
  if object `elem` (g ^. rock) then
    g & rock %~ (\list -> (V2 x (y-2)) : (delete object list))
  else g & monster %~ (\list -> (V2 x (y-2)) : (delete object list))

moveObject g MyEast = do
  let (V2 x y) = g ^. player
  let object = (V2 (x+1) y)
  -- delete curr_rock from the list, then insert new rock location into the list
  if object `elem` (g ^. rock) then
    g & rock %~ (\list -> (V2 (x+2) y) : (delete object list))
  else g & monster %~ (\list -> (V2 (x+2) y) : (delete object list))

moveObject g MyWest = do
  let (V2 x y) = g ^. player
  let object = (V2 (x-1) y)
  -- delete curr_rock from the list, then insert new rock location into the list
  if object `elem` (g ^. rock) then
    g & rock %~ (\list -> (V2 (x-2) y) : (delete object list))
  else g & monster %~ (\list -> (V2 (x-2) y) : (delete object list))

--delete an element from the list
delete :: Eq a => a -> [a] -> [a]
delete deleted list = [x | x <- list, x /= deleted]
