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

-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

data Cell = Snake | Food | Empty

data Cell2 = Empty2 | Player

-- App definition

app :: App Game2 Tick Name
app = App { appDraw = drawBoard2
          , appChooseCursor = neverShowCursor
          , appHandleEvent = testHandle
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
testHandle :: Game2 -> BrickEvent Name Tick -> EventM Name (Next Game2)
testHandle g _ = continue g


handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ turn North g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ turn South g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ turn East g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ turn West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ turn North g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ turn South g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue $ turn East g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'h') [])) = continue $ turn West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g ]

drawBoard :: Game -> [Widget Name]
drawBoard g = [drawGrid g]

drawBoard2 :: Game2 -> [Widget Name]
drawBoard2 g = [drawGrid2 g]

drawStats :: Game -> Widget Name
drawStats g = hLimit 11
  $ vBox [ drawScore (g ^. score)
         , padTop (Pad 2) $ drawGameOver (g ^. dead)
         ]

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawGameOver :: Bool -> Widget Name
drawGameOver dead =
  if dead
     then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
     else emptyWidget

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str "Board")
  $ vBox rows
  where
    --for each row
    rows         = [hBox (cellsInRow r) | r <- [height-1,height-2..0]]
    --forw each col
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    --for each coordinate decide to draw snake/food/empty
    drawCoord    = drawCell . cellAt
    cellAt c
      | c `elem`  g ^. snake = Snake
      | c == g ^. food      = Food
      | otherwise           = Empty

drawCell :: Cell -> Widget Name
drawCell Snake = withAttr snakeAttr cw
drawCell Food  = withAttr foodAttr cw
drawCell Empty = withAttr emptyAttr cw

drawGrid2 :: Game2 -> Widget Name
drawGrid2 g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "MyGame")
  $ vBox rows
  where
    rows = [hBox (cellsInRow r) | r <- [height - 1, height - 2 .. 0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord = drawCell2 . cellAt
    cellAt cell
      | cell == g ^. player     = Player
      | otherwise               = Empty2

drawCell2 :: Cell2 -> Widget Name
drawCell2 Empty2 = withAttr emptyAttr cw
drawCell2 Player = withAttr playerAttr cw


cw :: Widget Name
cw = str "  "

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (snakeAttr, V.blue `on` V.blue)
  , (foodAttr, V.red `on` V.red)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]

theMap2 :: AttrMap
theMap2 = attrMap V.defAttr
  [ (playerAttr, V.red `on` V.red)
  ]

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

snakeAttr, foodAttr, emptyAttr :: AttrName
snakeAttr = "snakeAttr"
foodAttr = "foodAttr"
emptyAttr = "emptyAttr"

playerAttr :: AttrName
playerAttr = "playerAttr"