--
-- COMP1100/1130, Semester 1, 2018
--
module State where

import ColourName
import Graphic
import Shape

data State = World [Graphic]
                   Tool
                   ColourName
  deriving (Show)

initialTool :: Tool
initialTool = RectangleTool Nothing

initialColour :: ColourName
initialColour = Black

initialState :: State
initialState = World [] initialTool initialColour

changeTool :: State -> Tool -> State
changeTool (World gs _ cn) tool = World gs tool cn

changeColorName :: State -> ColourName -> State
changeColorName (World gs tool _) cn = World gs tool cn

emptyGraphics :: State -> State
emptyGraphics (World _ tool cn) = World [] tool cn