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
drawNewGraphic (World gs (PolygonTool ps) cn) Maybe np = World gs (PolygonTool ps ++ [np]) cn -- handle Polygon firstly
drawNewGraphic (World gs (tool Nothing) cn) Maybe np = World gs (tool (Just np) cn            -- No old point
drawNewGraphic (World gs (RectangleTool Maybe op) cn) Maybe np = World gs ++ [getRectangleGraphic op np cn] (tool Nothing) cn
drawNewGraphic (World gs (EllipseTool Maybe op) cn) Maybe np = World gs ++ [getEllipseGraphic op np cn] (tool Nothing) cn
drawNewGraphic (World gs (LineTool Maybe op) cn) Maybe np = World gs ++ [getLineGraphic op np cn] (tool Nothing) cn

getNewGraphic :: State -> Maybe Point -> Maybe Graphic
getNewGraphic = undefined -- TODO

getRectangleGraphic :: Point -> Point -> ColourName -> Graphic
getRectangleGraphic p1 p2 cn = Graphic (Rectangle (fst (getRectangleHeightWidth p1 p2)) (snd (getRectangleHeightWidth p1 p2))) cn (getRectangleShift p1 p2)

getEllipseGraphic :: Point -> Point -> ColourName -> Graphic
getEllipseGraphic p1 p2 cn = Graphic (Ellipse (fst (getRectangleHeightWidth p1 p2)) (snd (getRectangleHeightWidth p1 p2))) cn (getRectangleShift p1 p2)

getLineGraphic :: Point -> Point -> ColourName -> Graphic
getLineGraphic p1 p2 cn = Graphic (Line p1 p2) cn p1

getPolygonGraphic :: [Point] -> ColourName -> Graphic
getPolygonGraphic ps cn = Graphic (Polygon ps) cn (getPolygonShift ps)

getWidthHeightShift :: Point -> Point -> (Side, Side, Point)
getWidthHeightShift p1 p2 = (fst (getRectangleHeightWidth p1 p2), snd (getRectangleHeightWidth p1 p2), (getRectangleShift p1 p2))

-- Normal pattern matching from customer Shape information to Picture
shapeToPic :: Shape -> Picture
shapeToPic (Rectangle w h) = rectangle w h
shapeToPic (Ellipse w h) = polygon (getEllipsePoints w h)
shapeToPic (Polygon ps) = polygon ps
shapeToPic (Line p1 p2) = polyline ([p1] ++ [p2]) -- pass test

graphicsToPics :: [Graphic] -> [Picture]
graphicsToPics = map graphicToPic

graphicToPic :: Graphic -> Picture
graphicToPic (Graphic shape cn ps) = translated (fst ps) (snd ps) (coloured (colourNameToColour cn) (shapeToPic shape))

-- get leftTop and rightBottom points to get width and height
getRectangleHeightWidth :: Point -> Point -> (Double, Double) 
getRectangleHeightWidth p1 p2 = ((abs ((snd p1) - (snd p2))), (abs ((fst p1) - (fst p2))))

-- get leftTop and rightBottom points to get width and height
getRectangleShift :: Point -> Point -> (Double, Double) 
getRectangleShift p1 p2 = (((snd p1) + (snd p2))/2, ((fst p1) + (fst p2))/2)

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

getPolygonShift :: [Point] -> Point
getPolygonShift ps = (((sum (map fst ps)) / ((getLength ps) * 1.0)), ((sum (map snd ps)) / ((getLength ps) * 1.0)))

getLength :: [Point] -> Double
getLength [] = 0.0
getLength (_:xs) = 1.0 + getLength xs