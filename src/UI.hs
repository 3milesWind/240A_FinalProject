{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Snake
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


data Cell2 = Empty2 | Player | Princess | Unwalkable

-- App definition

app :: App Game2 Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent2
          , appStartEvent = return
          , appAttrMap = const theMap2
          }

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  g <- initGame2
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g

-- Handling events

handleEvent2 :: Game2 -> BrickEvent Name Tick -> EventM Name (Next Game2)
handleEvent2 g (VtyEvent (V.EvKey V.KUp []))         = continue $ moves MyNorth g
handleEvent2 g (VtyEvent (V.EvKey V.KDown []))       = continue $ moves MySouth g
handleEvent2 g (VtyEvent (V.EvKey V.KLeft []))       = continue $ moves MyWest g
handleEvent2 g (VtyEvent (V.EvKey V.KRight []))      = continue $ moves MyEast g
handleEvent2 g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame2) >>= continue
handleEvent2 g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent2 g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent2 g _                                     = continue g  
-- Drawing

drawUI :: Game2 -> [Widget Name]
drawUI g =
  [ C.center $ padRight (Pad 5) (drawStats g) <+> drawGrid2 g ]



drawStats :: Game2 -> Widget Name
drawStats g = hLimit 20
  $ vBox [ drawSteps (g ^. stepsRemain)
         , padTop (Pad 2) $ drawQuit
         , padTop (Pad 2) $ drawRestart
         , padTop (Pad 2) $ drawGameOver2 (g ^. gameOver)
         , padTop (Pad 2) $ drawGameWin (g ^. win)
         ]


drawSteps :: Int -> Widget Name
drawSteps n = withAttr steps $ C.hCenter $ str ("Steps: " ++ (show n))

drawGameOver2 :: Bool -> Widget Name
drawGameOver2 dead =
  if dead
     then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
     else emptyWidget

drawGameWin :: Bool -> Widget Name
drawGameWin win =
  if win
    then withAttr gameWinAttr $ C.hCenter $ str "Sccess!"
    else emptyWidget

drawQuit :: Widget Name
drawQuit = withAttr quit $ C.hCenter $ str "Press q to quit"

drawRestart = withAttr restart $ C.hCenter $ str "Pree r to restart"

drawGrid2 :: Game2 -> Widget Name
drawGrid2 g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "MyGame")   
  $ vBox rows
  where
    rows = [hBox (cellsInRow r) | r <- [myheight - 1, myheight - 2 .. 0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..mywidth-1]]
    drawCoord = drawCell2 . cellAt
    cellAt cell
      | cell == (g ^. player)     = Player
      | cell == (g ^. princess)   = Princess
      | cell `elem` (g ^. unwalkable) = Unwalkable
      | otherwise                 = Empty2

drawCell2 :: Cell2 -> Widget Name
drawCell2 Empty2   = withAttr emptyAttr cw
drawCell2 Player   = withAttr playerAttr cw
drawCell2 Princess = withAttr princessAttr cw
drawCell2 Unwalkable = withAttr unwalkableAttr cw


cw :: Widget Name
cw = str "     \n\n\n" 


theMap2 :: AttrMap
theMap2 = attrMap V.defAttr
  [ (playerAttr, V.white `on` V.white)
  , (princessAttr, V.blue `on` V.blue)
  , (unwalkableAttr, V.black `on` V.black)
  , (emptyAttr, V.red `on` V.red)
  ]

gameOverAttr, gameWinAttr :: AttrName
gameOverAttr = "gameOver"
gameWinAttr = "gameWin"

snakeAttr, foodAttr, emptyAttr :: AttrName
snakeAttr = "snakeAttr"
foodAttr = "foodAttr"
emptyAttr = "emptyAttr"

playerAttr, princessAttr :: AttrName
playerAttr = "playerAttr"
princessAttr = "princessAttr"
unwalkableAttr = "unwalkableAttr"

steps :: AttrName
steps = "steps"

quit, restart :: AttrName
quit = "quit"
restart = "restart"