--
-- COMP1100/1130, Semester 1, 2018
--

{-# LANGUAGE OverloadedStrings #-}

module Events where

import CodeWorld hiding (trace)
import Debug.Trace
import State
import Shape
import ColourName
import Data.Text

--
-- | initialState Test
--
-- >>> initialState
-- World [] (RectangleTool Nothing) Black

--
-- | Rectangle Event Test
--
-- >>> handleEvent (KeyPress "R") initialState
-- World [] (RectangleTool Nothing) Black

--
-- | Incorrect Shape Input Test
--
-- >>> handleEvent (KeyPress "X") initialState
-- World [] (RectangleTool Nothing) Black

--
-- | Magenta Colour Event Test
--
-- >>> let state = World [] (RectangleTool Nothing) Black
-- >>> handleEvent (KeyPress "M") state
-- World [] (RectangleTool Nothing) Magenta

--
-- | Green Colour Event Test
--
-- >>> let state = World [] (RectangleTool Nothing) Black
-- >>> handleEvent (KeyPress "G") state
-- World [] (RectangleTool Nothing) Green

--
-- | Incorrect Colour Input Test
--
-- >>> let state = World [] (RectangleTool Nothing) Black
-- >>> handleEvent (KeyPress "H") state
-- World [] (RectangleTool Nothing) Black

--
-- | MousePress Event Test
--
-- >>> let state = World [] (RectangleTool Nothing) Cyan
-- >>> handleEvent (MousePress LeftButton (-3, 3)) state
-- World [] (RectangleTool (Just (-3.0,3.0))) Cyan

--
-- | MouseRelease Event Test
--
-- >>> let state = World [] (RectangleTool (Just (-3.0,3.0))) Cyan
-- >>> handleEvent (MouseRelease LeftButton (3, -3)) state
-- World [Graphic (Rectangle 6.0 6.0) Cyan (0.0,0.0)] (RectangleTool Nothing) Cyan

--
-- | Polygon Event Test
--
-- >>> handleEvent (KeyPress "P") initialState
-- World [] (PolygonTool []) Black

--
-- | Polygon Colour Test
--
-- >>> let state = World [] (PolygonTool []) Black
-- >>> handleEvent (KeyPress "O") state
-- World [] (PolygonTool []) Orange

--
-- | Polygon Vertex 1 Test
--
-- >>> let state = World [] (PolygonTool []) Orange
-- >>> handleEvent (MousePress LeftButton (3, 5)) state
-- World [] (PolygonTool [(3.0,5.0)]) Orange

--
-- | Polygon Vertex 2 Test
--
-- >>> let state = World [] (PolygonTool [(3.0,5.0)]) Green
-- >>> handleEvent (MousePress LeftButton (6, 8)) state
-- World [] (PolygonTool [(6.0,8.0),(3.0,5.0)]) Green

--
-- | Polygon Vertex 3 Test
--
-- >>> let state = World [] (PolygonTool [(6.0,8.0),(3.0,5.0)]) Green
-- >>> handleEvent (MousePress LeftButton (9, 2)) state
-- World [] (PolygonTool [(9.0,2.0),(6.0,8.0),(3.0,5.0)]) Green

--
-- | Polygon Draw Test
--
-- >>> let state = World [] (PolygonTool [(9.0,2.0),(6.0,8.0),(3.0,5.0)]) Yellow
-- >>> handleEvent (KeyPress " ") state
-- World [Graphic (Polygon [(9.0,2.0),(6.0,8.0),(3.0,5.0)]) Yellow (0.0,0.0)] (PolygonTool [(9.0,2.0),(6.0,8.0),(3.0,5.0)]) Yellow

--
-- | Shape Removal Test
--
-- >>> import Graphic
-- >>> let state = World [(Graphic (Rectangle 6 6) Cyan (0, 0)), (Graphic (Rectangle 3 3) Magenta (8, 4))] (RectangleTool Nothing) Orange
-- >>> handleEvent (KeyPress "Backspace") state
-- World [Graphic (Rectangle 3.0 3.0) Magenta (8.0,4.0)] (RectangleTool Nothing) Orange

-- 1) deconstructed the State -> World [Graphic] Tool ColourName
-- 2) list all possible input, or keep current state - from toolKeyMap & colourKeyMap
-- 3) if input in toolKeyMap, map to Tool
-- 4) if input in colourKeyMap, map to ColorName
-- 5) if the input is MousePress, we extract MouseButton and Point info 
-- 6) based on different shapes, we should pass different kinds of info to drawNewGraphic
-- 7) bind the drawNewGraphic to mouseRelease event
-- 8) remember to re-order the polygon
-- 9) remember to remove graphic

-- The Tool input is Maybe Point | [Point]

handleEvent :: Event -> State -> State
handleEvent e s =
  case e of
    KeyPress key
      | key == "Esc"         -> initialState
      | key == "D"           -> trace (show s) s
      | key == "H"           -> trace (show "Hello World") s
      | inMap toolKeyMap (unpack key) -> changeTool s (getValue toolKeyMap (unpack key))
      | inMap colourKeyMap (unpack key) -> changeColorName s (getValue colourKeyMap (unpack key))
    MousePress _ point  -> drawNewGraphic s (Maybe point)
    _ -> s

inMap :: [(String, a)] -> String -> Bool
inMap [] _ = False
inMap (x:xs) key = if key == fst x
                      then True
                      else inMap xs key

getValue :: [(String, a)] -> String -> a
getValue [] _ = error "Not find"
getValue (x:xs) key = if key == fst x
                      then snd x
                      else getValue xs key

