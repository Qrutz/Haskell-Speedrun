-- This module is a starting point for implementing the Graph Drawing
-- Calculator as described in Part II of the Standard Lab. You can use this
-- directly, or just study it as an example of how to use threepenny-gui.

import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import Expr
import Data.Maybe


canWidth,canHeight :: Num a => a
canWidth  = 400
canHeight = 400

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     canvas  <- mkCanvas canWidth canHeight   -- The drawing area
     fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
     input   <- mkInput 20 "x"                -- The formula input
     draw    <- mkButton "Draw graph"         -- The draw button
     scale   <- mkHTML "<i>scale</i>="
     sInput  <- mkInput 20 "1"
     zoomB   <- mkButton "Zoom"
     differ <-  mkButton "Differentiate"
     --coord   <- (150.0,150.0)                 -- Defaul Coordinates
       -- The markup "<i>...</i>" means that the text inside should be rendered
       -- in italics.

     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx,pure input]
     scaling <- row [pure scale,pure sInput]
     getBody window #+ [column [pure canvas,pure formula ,row [pure draw, pure differ],row [pure scaling,pure zoomB]]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center"),("display", "flex"),("justify-content", "center"),("margin-top", "100px"),("text-align","-webit-center")]
     pure input # set style [("fontSize","14pt"), ("text-align","-webit-center")]

     pure sInput # set style [("fontSize","14pt")]

     pure canvas # set style [("text-align","-webit-center")]

     pure draw # set style [("margin-left", "97px"), ("margin-top", "6px")]

     pure differ # set style [("margin-left", "20px"), ("margin-top", "7px")]

     pure scaling # set style [("margin-top", "12px")]

     pure zoomB # set style [("margin-left", "20px")]

     -- Interaction (install event handlers)
     on UI.click     draw  $ \ _ -> readAndDraw input canvas
     on valueChange' input $ \ _ -> readAndDraw input canvas
     on UI.mousedown canvas $ \coord -> zoom input sInput coord canvas

     on UI.click     differ $ \ _ -> do
       formula <- get value input
       pure input # set value  (diff formula)
       readAndDraw input canvas

diff :: String -> String
diff formula = showExpr $ differentiate $ fromJust $ readExpr formula

zoom :: Element -> Element -> Point -> Canvas -> UI ()
zoom input sInput coord canvas =
  do -- Get the current formula (a String) from the input element
     formula <- get value input
     scale   <- get value sInput
     -- Clear the canvas
     clearCanvas canvas
     -- The following code draws the formula text in the canvas and a blue line.
     -- It should be replaced with code that draws the graph of the function.
     set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
     UI.fillText formula (10,canHeight/2) canvas
     path "blue" (points (fromJust $ readExpr formula) (read scale) (canWidth,canHeight)) canvas

readAndDraw :: Element -> Canvas -> UI ()
readAndDraw input canvas =
  do -- Get the current formula (a String) from the input element
     formula <- get value input
     -- Clear the canvas
     clearCanvas canvas
     -- The following code draws the formula text in the canvas and a blue line.
     -- It should be replaced with code that draws the graph of the function.
     set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
     UI.fillText formula (10,canHeight/2) canvas
     path "blue" (points (fromJust $ readExpr formula) 1 (canWidth,canHeight)) canvas




points :: Expr -> Double -> (Int,Int) -> [Point]
points exp scale (width,height) = [(canWidth/2 + x,canHeight/2 - eval exp x) | x <- xAxis]
  where xAxis = [scale* fromIntegral (-width)..scale*fromIntegral width]
