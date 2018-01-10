import Utils
import Logic
import Loader
import Defaults

import Brick
import Brick.Widgets.Core
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Util
import Graphics.Vty

import Control.Monad
import Data.List
import Lens.Micro
import System.Environment 
import System.Directory 

app :: App Game e ()
app = App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent  = handleEvent
    , appStartEvent   = return
    , appAttrMap      = const theMap
    }

doneLook   = "  "
dEmptyLook = "  "
fullLook   = "{}"
markedLook = "XX"
emptyLook  = "[]"

doneAttr    = attrName "doneAttr"
dEmptyAttr  = attrName "dEmptyAttr"

fullAttr  = attrName "fullAttr"
sFullAttr = attrName "sFullAttr"

markedAttr  = attrName "markedAttr"
sMarkedAttr = attrName "sMarkedAttr"

emptyAttr  = attrName "emptyAttr"
sEmptyAttr = attrName "sEmptyAttr"

theMap = attrMap defAttr
    [ (doneAttr  , bg white)
    , (dEmptyAttr, fg black)

    , (fullAttr , fg blue)
    , (sFullAttr, black `on` blue)

    , (markedAttr , fg red)
    , (sMarkedAttr, black `on` red)

    , (emptyAttr , fg white)
    , (sEmptyAttr, black `on` white)
    ]

drawUI g = [ drawUsage <=> (hCenter $ padTop (Pad 4) $ drawSolved g)
           , decorate
           ]
  where
    decorate
        | g^.state == Finished = center $ drawGrid g
        | otherwise            = decorations (drawGrid)
    decorations f = center $ (drawVerticalHints g) <=> ((drawHorizontalHints g) <+> (border $ f g))

drawUsage = vBox $ map str 
    [ "Arrow keys to navigate grid."
    , "'F' to fill a block."
    , "'X' to mark a block."
    , "'C' to check if the grid is solved."
    , "'Q' to quit."
    ]

drawSolved g@(Game _ _ _ s) = border $ padLeftRight 4 $ padTopBottom 1 $ str status
  where
    status
        | s == Finished = " Solved!"
        | otherwise     = " Not solved."

drawHorizontalHints (Game g _ _ _) = border $ vBox $ map str $ strings
  where
    check []        = [0]
    check xs        = xs
    hints           = map check $ genLineLengths g
    alignStrings xs = map (align (longestLength xs) ' ') xs
    strings         = alignStrings $ map (unwords . (map show)) hints

drawVerticalHints (Game g _ _ _) = formatting $ map ((' ':) . intersperse ' ') filtered
  where
    check []        = [0]
    check xs        = xs
    hints           = map check $ genLineLengths $ rotate g
    padding         = longestLength $ map (unwords . (map show)) $ genLineLengths g
    alignStrings xs = map (align (longestLength xs) ' ') xs
    strings         = alignStrings $ map (unwords . (map show)) hints
    filtered        = filter ((/="") . (filter (/=' '))) $ rotate strings
    formatting xs   = padLeft (Pad $ padding + 2) $ border $ vBox $ map str xs

drawGrid (Game _ gr c s) = vBox $ map drawLine (zip gr [0..length gr - 1])
  where
    drawLine (l, y)    = hBox $ map (drawBlock y) (zip l [0..length l - 1])
    drawBlock y (b, x)
        | s == Unfinished = withAttr attr $ str nLook
        | otherwise       = withAttr dAttr $ str dLook
      where
        nLook
            | b == Full   = fullLook
            | b == Marked = markedLook
            | b == Empty  = emptyLook
        dLook
            | b == Full   = doneLook
            | b == Marked = dEmptyLook
            | b == Empty  = dEmptyLook
        nAttr
            | b == Full   = fullAttr
            | b == Marked = markedAttr
            | b == Empty  = emptyAttr
        sAttr
            | b == Full   = sFullAttr
            | b == Marked = sMarkedAttr
            | b == Empty  = sEmptyAttr
        dAttr
            | b == Full   = doneAttr
            | b == Marked = emptyAttr
            | b == Empty  = emptyAttr
        attr
            | c == (x, y) = sAttr
            | otherwise   = nAttr

handleEvent g (VtyEvent (EvKey (KChar 'f') [])) = continue $ run (updateBlock Full) g
handleEvent g (VtyEvent (EvKey (KChar 'x') [])) = continue $ run (updateBlock Marked) g
handleEvent g (VtyEvent (EvKey KUp []))         = continue $ run (moveCursor UpDir) g
handleEvent g (VtyEvent (EvKey KRight []))      = continue $ run (moveCursor RightDir) g
handleEvent g (VtyEvent (EvKey KDown []))       = continue $ run (moveCursor DownDir) g
handleEvent g (VtyEvent (EvKey KLeft []))       = continue $ run (moveCursor LeftDir) g
handleEvent g (VtyEvent (EvKey (KChar 'c') [])) = continue $ run check g
handleEvent g (VtyEvent (EvKey (KChar 'q') [])) = halt g
handleEvent g _                                 = continue g

dispatch = [ ("-h"    , usage)
           , ("--help", usage)
           , (""      , runGame noArgGrid)
           ]

usage = do
    self <- getProgName

    putStr $ unlines $
        "":
        concat ["Usage: ", self, " [OPTION]"]:
        "   -h,--help           Print this message":
        "   [FILENAME]          Loads [FILENAME] as goal":
        "":
        []

runGame gr = do
    defaultMain app (Game
        { _goal   = gr
        , _grid   = genEmpty (length $ head gr, length gr)
        , _cursor = (0, 0)
        , _state  = Unfinished
        })
    return ()

main = do
    a <- getArgs
    exists <- doesFileExist $ head $ a ++ [""]

    let args       = a ++ [""]
        dispatched = lookup (head args) dispatch
        check Nothing
            | not exists = runGame noArgGrid
            | otherwise  = do
                file <- readFile $ head args
                runGame $ parseGrid file
        check (Just action) = action

    check dispatched
