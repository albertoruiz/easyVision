-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.GUI.Util
Copyright   :  (c) Alberto Ruiz 2012
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

common interfaces

-}
-----------------------------------------------------------------------------

module EasyVision.GUI.Util (
    observe,
    sMonitor,
    browser,
    editor,
    updateItem
) where

import Graphics.UI.GLUT hiding (Point,Size)
import EasyVision.GUI.Types
import EasyVision.GUI.Interface
import Control.Arrow((***))
import ImagProc.Base
import Util.Misc(replaceAt)

editor :: [Command (Int,[x]) (Int,[x])] -> [Command (Int,[x]) (IO())]
       -> String -> [x] -> (Int -> x -> Drawing) -> IO (EVWindow (Int,[x]))
editor upds acts name xs drw = standalone (Size 300 300) name (0,xs) (upds ++ move) acts f
  where
    f (k,xs) | null xs = text (Point 0 0) "empty list!"
             | otherwise = drw j (xs !! j)
                 where
                   j = k `mod` (length xs)   
    move = g2 [((MouseButton WheelUp,   Down, modif), (+1))
              ,((SpecialKey  KeyUp,     Down, modif), (+1))
              ,((MouseButton WheelDown, Down, modif), pred)
              ,((SpecialKey  KeyDown,   Down, modif), pred)]
    g2 = map (id *** const.const.(***id))

--    g1 = map (id *** const.const.(snd.))
--    g3 = map (id *** const.const)


--updateItem :: key -> (a -> a) -> (key, roi -> pt -> (Int, [a]) -> (Int, [a]))
updateItem key f = (key, \roi pt (k,xs) -> (k, replaceAt [k] [f roi pt (xs!!k)] xs))

--------------------------------------------------------------------------------

browser :: String -> [x] -> (Int -> x -> Drawing) -> IO (EVWindow (Int,[x]))
browser = editor [] [] 

--------------------------------------------------------------------------------

sMonitor :: String -> (WinRegion -> b -> [Drawing]) -> VC b b
sMonitor name f = interface (Size 240 360) name 0 (const.const.return $ ()) (c2 acts) [] (const (,)) g
  where
    g roi k x = r !! j
      where
        r = f roi x
        j = k `mod` length r
    acts = [((MouseButton WheelUp,   Down, modif), (+1))
           ,((SpecialKey  KeyUp,     Down, modif), (+1))
           ,((MouseButton WheelDown, Down, modif), pred)
           ,((SpecialKey  KeyDown,   Down, modif), pred)]
    c2 = Prelude.map (id *** const.const)

--------------------------------------------------------------------------------

observe :: Renderable x => String -> (b -> x) -> VC b b
observe name f = interface (Size 240 360) name () (const.const.return $ ()) [] [] (const (,)) (const.const $ Draw . f)

