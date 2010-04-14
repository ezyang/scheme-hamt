import Text.CSV
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Data.Accessor
import System.Environment
import Data.Maybe
import Data.Colour.Names
import Data.Colour
import Control.Monad
import qualified Data.Map as Map

main = do
    args <- getArgs
    case args of
        x:[] -> run x (x++".csv") (x++".pdf")
        otherwise -> putStrLn "Usage: runhaskell graph.hs NAME"

run name filename out = do
    r <- parseCSVFromFile filename
    either errorHandler (process name out) r

errorHandler = print

doubleRead "." = "."
doubleRead s@('.':_) = doubleRead' ('0':s)
doubleRead s = doubleRead' s
doubleRead' "." = ".0"
doubleRead' (c:cs) = c : doubleRead' cs
doubleRead' [] = []

process name out csv = do
    let tuples = map processRecord . filter ((==3).length) $ csv
        fulldata = Map.assocs $ foldl reduce Map.empty tuples
            where reduce m (s,i,v) = Map.insertWith' (flip (++)) s [(i,v)] m
        chart = layout1_title ^= name
              $ layout1_left_axis .> laxis_title ^= "ticks"
              $ layout1_bottom_axis .> laxis_title ^= "elements"
              $ layout1_plots ^= zipWith makePlots colors fulldata
              $ defaultLayout1
            where makePlots x y = Left . toPlot $ line x y
    renderableToPDFFile (toRenderable chart) 640 480 out
    where
        processRecord :: [String] -> (String, Int, Double)
        processRecord (x:y:z:[]) = (x, read y, read $ doubleRead z)

line color (name, ticks) = plot_lines_values ^= [ticks]
                         $ plot_lines_title ^= name
                         $ plot_lines_style .> line_color ^= color
                         $ defaultPlotLines

colors :: [AlphaColour Double]
colors = cycle $ map opaque [ blue, red, green, yellow, cyan, magenta, black ]
