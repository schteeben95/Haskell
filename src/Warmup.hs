--
-- COMP1100/1130, Semester 1, 2018
--

module Main where
import CodeWorld
import View
import Shape
import Graphic
import ColourName

ourPicture :: Picture
ourPicture = coordinatePlane & (shapeToPic (Ellipse 3 8))
-- ourPicture = coordinatePlane & (graphicToPic (getRectangleGraphic (1, -1) (-8, 3) Cyan))

main :: IO ()
main = drawingOf ourPicture
