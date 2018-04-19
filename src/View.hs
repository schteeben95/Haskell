--
-- COMP1100/1130, Semester 1, 2018
--
module View where

import CodeWorld
import ColourName
import Graphic
import Shape
import State

import qualified Data.Text as Text

drawState :: State -> Picture
drawState (World gs tool colour) =
  pictures $ shapeToPicture : colourToPicture : graphicsToPics gs
  where
    shapeToPicture, colourToPicture :: Picture
    shapeToPicture =
      translated (-13.5) 8 $ (text . Text.pack) ("Shape: " ++ shapeToText)
    colourToPicture =
      translated (-13.5) 7 $ (text . Text.pack) ("Colour: " ++ colourToText)
    shapeToText :: String
    shapeToText =
      let shape = takeWhile (/= ' ') $ show tool
       in take (length shape - 4) shape
    colourToText :: String
    colourToText = show colour

--
-- | drawNewGraphic Test 1
--
-- >>> drawNewGraphic (World [] (RectangleTool (Just (-3, 3))) Black) (Just (3, -3))
-- World [Graphic (Rectangle 6.0 6.0) Black (0.0,0.0)] (RectangleTool Nothing) Black
-- 
-- | drawNewGraphic Test 2
-- 
-- >>> drawNewGraphic (World [] (RectangleTool (Just (0, 0))) Orange) (Just (3, 7))
-- World [Graphic (Rectangle 3.0 7.0) Orange (1.5,3.5)] (RectangleTool Nothing) Orange
drawNewGraphic :: State -> Maybe Point -> State
drawNewGraphic (World gs (PolygonTool ps) cn) (Just np) = World gs (PolygonTool (ps ++ [np])) cn -- handle Polygon firstly
drawNewGraphic (World gs (RectangleTool Nothing) cn) np = World gs (RectangleTool np) cn            -- No old point
drawNewGraphic (World gs (EllipseTool Nothing) cn) np = World gs (EllipseTool np) cn            -- No old point
drawNewGraphic (World gs (LineTool Nothing) cn) np = World gs (LineTool np) cn            -- No old point
drawNewGraphic (World gs (RectangleTool (Just op)) cn) (Just np) = World (gs ++ [getRectangleGraphic op np cn]) (RectangleTool Nothing) cn
drawNewGraphic (World gs (EllipseTool (Just op)) cn) (Just np) = World (gs ++ [getEllipseGraphic op np cn]) (EllipseTool Nothing) cn
drawNewGraphic (World gs (LineTool (Just op)) cn) (Just np) = World (gs ++ [getLineGraphic op np cn]) (LineTool Nothing) cn

getNewGraphic :: State -> Maybe Point -> Maybe Graphic
getNewGraphic = undefined -- TODO

drawPolygon :: State -> State
drawPolygon (World gs (PolygonTool ps) cn) =  World (gs ++ [getPolygonGraphic ps cn]) (PolygonTool []) cn
drawPolygon s = s

getRectangleGraphic :: Point -> Point -> ColourName -> Graphic
getRectangleGraphic p1 p2 cn = Graphic (Rectangle (fst (getRectangleHeightWidth p1 p2)) (snd (getRectangleHeightWidth p1 p2))) cn (getShift ([p1] ++ [p2]))

getEllipseGraphic :: Point -> Point -> ColourName -> Graphic
getEllipseGraphic p1 p2 cn = Graphic (Ellipse (fst (getRectangleHeightWidth p1 p2)) (snd (getRectangleHeightWidth p1 p2))) cn (getShift ([p1] ++ [p2]))

getLineGraphic :: Point -> Point -> ColourName -> Graphic
getLineGraphic p1 p2 cn = Graphic (Line p1 p2) cn (0, 0)

getPolygonGraphic :: [Point] -> ColourName -> Graphic
getPolygonGraphic ps cn = Graphic (Polygon ps) cn (0, 0)

getWidthHeightShift :: Point -> Point -> (Side, Side, Point)
getWidthHeightShift p1 p2 = (fst (getRectangleHeightWidth p1 p2), snd (getRectangleHeightWidth p1 p2), (getShift [p1, p2]))

-- Normal pattern matching from customer Shape information to Picture
shapeToPic :: Shape -> Picture
shapeToPic (Rectangle w h) = solidRectangle w h
shapeToPic (Ellipse w h) = solidPolygon (getEllipsePoints w h)
shapeToPic (Polygon ps) = solidPolygon ps
shapeToPic (Line p1 p2) = polyline ([p1] ++ [p2]) -- pass test

graphicsToPics :: [Graphic] -> [Picture]
graphicsToPics = map graphicToPic

graphicToPic :: Graphic -> Picture
graphicToPic (Graphic shape cn (x, y)) = translated x y (coloured (colourNameToColour cn) (shapeToPic shape))

-- get leftTop and rightBottom points to get width and height
getRectangleHeightWidth :: Point -> Point -> (Double, Double) 
getRectangleHeightWidth p1 p2 = ((abs ((fst p1) - (fst p2))), (abs ((snd p1) - (snd p2))))

-- we may add a if else to swap the start and end point
getEllipsePoints :: Double -> Double -> [Point]
getEllipsePoints w h = (getAllPoints (w/2) (h/2) (-w/2) True) ++ (reverse (getAllPoints (w/2) (h/2) (-w/2) False))

getAllPoints :: Double -> Double -> Double -> Bool -> [Point]
getAllPoints w h cur flag = if cur >= w
                         then [] -- over the range
                         else [getEllipsePoint w h cur flag] ++ (getAllPoints w h (cur + w/10000) flag)

-- pass a width, height and a current x value, return a point
getEllipsePoint :: Double -> Double -> Double -> Bool -> Point
getEllipsePoint w h cur flag = if flag
                               then (cur, sqrt ((w**2) * (h**2) - (h**2) * (cur ** 2)) / w)
                               else (cur, -(sqrt ((w**2) * (h**2) - (h**2) * (cur ** 2)) / w))

getShift :: [Point] -> Point
getShift ps = (((sum (map fst ps)) / ((getLength ps) * 1.0)), ((sum (map snd ps)) / ((getLength ps) * 1.0)))

getLength :: [Point] -> Double
getLength [] = 0.0
getLength (_:xs) = 1.0 + getLength xs