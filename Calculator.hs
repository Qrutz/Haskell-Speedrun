-- This module is a starting point for implementing the Graph Drawing
-- Calculator as described in Part II of the Standard Lab. You can use this
-- directly, or just study it as an example of how to use threepenny-gui.

import ThreepennyPages
    ( mkButton,
      mkCanvas,
      mkHTML,
      mkInput,
      mkSlider,
      mkSlider',
      path,
      valueChange',
      clearCanvas,
      Canvas,
      Point,
      Element )
import Graphics.UI.Threepenny.Core as UI
    ( UI,
      defaultConfig,
      (#),
      (#+),
      column,
      get,
      getBody,
      on,
      row,
      set,
      style,
      value,
      startGUI,
      Window )
import qualified Graphics.UI.Threepenny as UI
import Expr
import Data.Maybe(fromJust)
import Control.Monad

-- * H

points :: Expr -> Double -> (Int,Int) -> [Point]
points e s (width,height) = let xs = [0..fromIntegral width]
                                ys = map (realToPix . eval e . pixToReal) xs
                                      in zip xs ys
  where
    pixToReal :: Double -> Double
    pixToReal x = s * (x - fromIntegral width / 2)

    realToPix :: Double -> Double
    realToPix y = (-y/s) + fromIntegral height / 2


-- * I

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
     scale   <- mkHTML "<i>scaling</i>="
     sInput  <- mkInput 10 "0.05"
     adder   <- mkButton  "+"
     decrease <- mkButton "-"


     differ <-  mkButton "Differentiate"
       -- The markup "<i>...</i>" means that the text inside should be rendered
       -- in italics.

     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx,pure input]
     scaling <- row [pure scale, pure decrease,pure sInput, pure adder]
     getBody window #+ [column [pure canvas,pure formula ,row [pure draw, pure differ],row [pure scaling]]]

     -- Styling

  
     
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center"),("display", "flex"),("justify-content", "center"),("margin-top", "100px"),("text-align","-webit-center")]
     pure input # set style [("fontSize","14pt"), ("text-align","-webit-center")]

     pure sInput # set style [("fontSize","14pt")]

     pure canvas # set style [("text-align","-webit-center")]

     pure draw # set style [("margin-left", "97px"), ("margin-top", "6px")]

     pure differ # set style [("margin-left", "20px"), ("margin-top", "7px")]

     pure scaling # set style [("margin-top", "12px")]

     pure adder # set style [("margin-left", "3px")]

     pure decrease # set style [("margin-right", "3px")]


     -- Interaction (install event handlers)
     -- Empty strings are for diff. expressions 
     on UI.click     draw  $ \ _ -> readAndDraw input sInput  canvas
     on valueChange' input $ \ _ -> readAndDraw input sInput  canvas
     on UI.click adder $ \ _ -> do
      scale <- get value sInput
      pure sInput # set value (scaleAdder scale "0.03")
      readAndDraw input sInput canvas

     on UI.click decrease $ \ _ -> do
       scale <- get value sInput
       pure sInput # set value (scaleDecreaser scale "0.03")
       readAndDraw input sInput canvas

     on UI.click differ $ \ _ -> do
       formula <- get value input
       pure input # set value (diff formula)
       readAndDraw' input sInput canvas

diff :: String -> String
diff formula = showExpr $ differentiate $ fromJust $ readExpr formula


readAndDraw :: Element -> Element -> Canvas -> UI ()
readAndDraw input sInput canvas  =
  do -- Get the current formula (a String) from the input elements
     formula <- get value input
     scale   <- get value sInput
     -- Clear the canvas
     clearCanvas canvas
     -- The following code draws the formula text in the canvas and a blue line.
     set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
     UI.fillText formula (10,canHeight/2) canvas
     path "blue" (points (checkExpr $ readExpr formula) (read scale) (canWidth,canHeight)) canvas


-- Same function as above with the change of LINE-COLOR -- 
readAndDraw' :: Element -> Element -> Canvas -> UI ()
readAndDraw' input sInput canvas  =
  do -- Get the current formula (a String) from the input elements
     formula <- get value input
     scale   <- get value sInput
     -- Clear the canvas
     clearCanvas canvas
     -- The following code draws the formula text in the canvas and a blue line.
     set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
     UI.fillText formula (10,canHeight/2) canvas
     path "red" (points (checkExpr $ readExpr formula) (read scale) (canWidth,canHeight)) canvas

-- Checks for invalid expressions
checkExpr :: Maybe Expr -> Expr
checkExpr Nothing = Num 0
checkExpr (Just n) = n

scaleAdder :: String -> String -> String
scaleAdder scale add = let x = read scale :: Double
                           y = read add :: Double
                           in  show (x + y)

scaleDecreaser :: String -> String -> String
scaleDecreaser "0" add = "0"
scaleDecreaser scale add = let x = read scale :: Double
                               y = read add :: Double
                               in  show (x - y)

