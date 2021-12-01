{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import MyGame
import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.))
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))

-- Types
data Tick = Tick


-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()


data Cell2 = Empty2 | Player | Princess | Unwalkable | Rock | Monster

-- App definition

app :: App Game2 Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent2
          , appStartEvent = return
          , appAttrMap = const theMap2
          }


tmp:: IO ()
tmp = do
    chan <- newBChan 10
    forkIO $ forever $ do
      writeBChan chan Tick
      threadDelay 100000 -- decides how fast your game moves
    g <- initGame1
    let builder = V.mkVty V.defaultConfig
    initialVty <- builder
    void $ customMain initialVty builder (Just chan) app g

tmp2:: IO ()
tmp2 = do
    chan <- newBChan 10
    forkIO $ forever $ do
      writeBChan chan Tick
      threadDelay 100000 -- decides how fast your game moves
    g <- initGame2
    let builder = V.mkVty V.defaultConfig
    initialVty <- builder
    void $ customMain initialVty builder (Just chan) app g

tmp3:: IO ()
tmp3 = do
    chan <- newBChan 10
    forkIO $ forever $ do
      writeBChan chan Tick
      threadDelay 100000 -- decides how fast your game moves
    g <- initGame3
    let builder = V.mkVty V.defaultConfig
    initialVty <- builder
    void $ customMain initialVty builder (Just chan) app g

main2 :: Int -> IO ()
main2 int= if int ==1 
            then tmp
           else if int == 2
             then tmp2
           else tmp3
            

-- Handling events

handleEvent2 :: Game2 -> BrickEvent Name Tick -> EventM Name (Next Game2)
handleEvent2 g (VtyEvent (V.EvKey V.KUp []))         = continue $ moves MyNorth g
handleEvent2 g (VtyEvent (V.EvKey V.KDown []))       = continue $ moves MySouth g
handleEvent2 g (VtyEvent (V.EvKey V.KLeft []))       = continue $ moves MyWest g
handleEvent2 g (VtyEvent (V.EvKey V.KRight []))      = continue $ moves MyEast g
handleEvent2 g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (handleRestart g) >>= continue
handleEvent2 g (VtyEvent (V.EvKey (V.KChar '1') [])) = liftIO (initGame1) >>= continue
handleEvent2 g (VtyEvent (V.EvKey (V.KChar '2') [])) = liftIO (initGame2) >>= continue
handleEvent2 g (VtyEvent (V.EvKey (V.KChar '3') [])) = liftIO (initGame3) >>= continue
handleEvent2 g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent2 g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent2 g _                                     = continue g  

handleRestart :: Game2 -> IO Game2
handleRestart g = 
  if g ^. level == 1
    then initGame1
  else if g ^. level == 3
    then initGame3
  else initGame2
-- Drawing

drawUI :: Game2 -> [Widget Name]
drawUI g =
  [ C.center $ padRight (Pad 5) (drawStats g) <+> drawGrid2 g ]



drawStats :: Game2 -> Widget Name
drawStats g = hLimit 20
  $ vBox [ drawSteps (g ^. stepsRemain)
         , padTop (Pad 2) $ drawQuit
         , padTop (Pad 2) $ drawRestart (g ^. win)
         , padTop (Pad 2) $ drawGameOver2 (g ^. gameOver) (g ^. win)
         , padTop (Pad 2) $ drawGameWin (g ^. win)
         , padTop (Pad 2) $ drawLevel1 (g ^. level)
         , padTop (Pad 2) $ drawLevel2 (g ^. level)
         , padTop (Pad 2) $ drawLevel3 (g ^. level)
         ]


drawSteps :: Int -> Widget Name
drawSteps n = withAttr steps $ C.hCenter $ str ("Steps: " ++ (show n))

drawGameOver2 :: Bool -> Bool -> Widget Name
drawGameOver2 dead win =
  if dead && (win == False)
     then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
  else emptyWidget

drawGameWin :: Bool -> Widget Name
drawGameWin win =
  if win
    then withAttr gameWinAttr $ C.hCenter $ str "Success!"
else emptyWidget

drawQuit :: Widget Name
drawQuit = withAttr quit $ C.hCenter $ str "Press q to quit"

drawRestart :: Bool ->  Widget Name
drawRestart win = 
  if win then 
    emptyWidget
  else
    withAttr restart $ C.hCenter $ str "Press r to restart"

drawLevel1 :: Int ->  Widget Name
drawLevel1 level = 
  if level == 1
    then emptyWidget
  else withAttr level1 $ C.hCenter $ str "Press 1 to level1"

drawLevel2 :: Int -> Widget Name
drawLevel2 level = 
  if level == 2
    then emptyWidget
  else withAttr level2 $ C.hCenter $ str "Press 2 to level2"

drawLevel3 :: Int -> Widget Name
drawLevel3 level =
  if level == 3
    then emptyWidget
  else withAttr level3 $ C.hCenter $ str "Press 3 to level3"

drawGrid2 :: Game2 -> Widget Name
drawGrid2 g =
  if (g ^. level /= 3) then (drawGameLevel g)
  else drawGameLevel3 g

drawGameLevel :: Game2 -> Widget Name
drawGameLevel g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str ("Rescue Princess " ++ "level: " ++ show (g ^. level)))   
  $ vBox rows
  where
    rows = [hBox (cellsInRow r) | r <- [myheight - 1, myheight - 2 .. 0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..mywidth-1]]
    drawCoord = drawCell2 . cellAt
    cellAt cell
      | cell == (g ^. player)         = Player
      | cell == (g ^. princess)       = Princess
      | cell `elem` (g ^. unwalkable) = Unwalkable
      | cell `elem` (g ^. rock)       = Rock
      | cell `elem` (g ^. monster)    = Monster
      | otherwise                     = Empty2

drawGameLevel3 :: Game2 -> Widget Name
drawGameLevel3 g =  withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str ("Rescue Princess " ++ "level: " ++ show (g ^. level)))   
  $ vBox rows
  where
    rows = [hBox (cellsInRow r) | r <- [level3_height - 1, level3_height - 2 .. 0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..level3_width - 1]]
    drawCoord = drawCell2 . cellAt
    cellAt cell
      | cell == (g ^. player)         = Player
      | cell == (g ^. princess)       = Princess
      | cell `elem` (g ^. unwalkable) = Unwalkable
      | cell `elem` (g ^. rock)       = Rock
      | cell `elem` (g ^. monster)    = Monster
      | otherwise                     = Empty2

drawCell2 :: Cell2 -> Widget Name
drawCell2 Empty2   = withAttr emptyAttr cw
drawCell2 Player   = withAttr playerAttr playercw
drawCell2 Princess = withAttr princessAttr princesscw
drawCell2 Unwalkable = withAttr unwalkableAttr cw
drawCell2 Rock = withAttr rockAttr rockcw
drawCell2 Monster = withAttr monsterAttr monstercw

cw :: Widget Name
cw = str "          \n\n\n\n\n" 


princesscw :: Widget Name
princesscw = str " --♚♚♚--||\n｜     ||\n｜ ♥ ♥ ||\n｜  - _|_|\n\n"    

monstercw :: Widget Name
monstercw = str "  _   _  \n -☻-  -☻- \n  |    |  \n    ^^   \n   ▾▾▾▾  \n" 

playercw :: Widget Name
playercw = str " -------//\n｜     /./\n｜ _ _ /./\n｜  - _/_/\n\n"  

rockcw :: Widget Name
rockcw = str "==========\n|        |\n|        |\n|        |\n==========" 

theMap2 :: AttrMap
theMap2 = attrMap V.defAttr
  [ (playerAttr, V.blue `on` V.white)
  , (princessAttr, V.green `on` V.white)
  , (unwalkableAttr, V.black `on` V.black)
  , (emptyAttr, V.white `on` V.white)
  , (rockAttr, V.black `on` V.white)
  , (monsterAttr, V.red `on` V.white)
  ]

gameOverAttr, gameWinAttr :: AttrName
gameOverAttr = "gameOver"
gameWinAttr = "gameWin"

snakeAttr, foodAttr, emptyAttr :: AttrName
snakeAttr = "snakeAttr"
foodAttr = "foodAttr"
emptyAttr = "emptyAttr"

playerAttr, princessAttr, unwalkableAttr, rockAttr, monsterAttr :: AttrName
playerAttr = "playerAttr"
princessAttr = "princessAttr"
unwalkableAttr = "unwalkableAttr"
rockAttr = "rockAttr"
monsterAttr = "monsterAttr"

steps :: AttrName
steps = "steps"

quit, restart, level1, level2, level3 :: AttrName
quit = "quit"
restart = "restart"
level1 = "level1"
level2 = "level2"
level3 = "level3"
