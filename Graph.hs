module Graph where

import Graphics.Blank
import Control.Arrow hiding (loop)
import System.Random
import GHC.Float
import LinReg
import Data.List

lsrf :: Int -> [(Float,Float)] -> [Float]
lsrf o l = map double2Float $ lsr o (map (float2Double *** float2Double) l)

sketch :: ((Float,Float) -> (Float,Float)) -> [(Float,Float)] -> Canvas ()
sketch f l = beginPath () >> moveTo (f $ head l) >> mapM_ (lineTo . f) l

dot :: ((Float,Float) -> (Float,Float)) -> (Float,Float) -> Canvas ()
dot f p = do
  save ()
  beginPath ()
  translate (f p)
  arc (0, 0, 8, 0, 2 * pi, False)
  strokeStyle "#2222DD"
  lineWidth 4
  stroke
  restore ()

adjust :: (Float, Float) -> [(Float, Float)] -> (Float, Float) -> (Float, Float)
adjust (w,h) l = f
  where
    ((lx,ly),(hx,hy)) = bounds l
    dx = hx - lx
    dy = hy - ly
    f (x,y) = (w * (x - lx) / dx, h * (hy - y) / dy)

bounds :: [(Float,Float)] -> ((Float,Float),(Float,Float))
bounds = ((minimum *** minimum) &&& (maximum *** maximum)) . unzip

annealedData :: Float -> [Float] -> IO [(Float, Float)]
annealedData m constants = do
  gen <- newStdGen
  return $ zipWith f (randomRs (0,m) gen) (functionData constants)

  where f a (x,y) = (x, a+y-(m/2))

functionData :: [Float] -> [(Float, Float)]
functionData cs = map (id &&& poly cs) [0..]

poly :: [Float] -> Float -> Float
poly cs x = sum $ zipWith (*) cs $ map (x**) [0..]

colors :: [String]
colors = cycle $ words "#CC3300 #CC9900 #99CC00 #33CC00 #00CC33 #00CC99 #0099CC #0033CC #470AFF #7547FF #C2FF0A"

main :: IO ()
main = do points <- fmap (take 18) $ annealedData 5000 [0,8,-3,1,0,-0.1]

          blankCanvas 5001 $ \ context -> do

            screenSize@(x,y) <- send context size

            let adjuster = adjust screenSize (concat $ points : fits)
                cs       = flip map [2..8] $ \x -> lsrf x points
                fits     = map (\c -> map (id &&& poly c) (map fst points)) cs

            flip mapM_ (take 3 $ zip4 [0..] cs colors fits) $ \(i, c, color, fit) -> do
              send context $ do
                strokeStyle color
                sketch adjuster fit
                lineWidth 4
                stroke

                save ()
                fillStyle color
                font "15pt arial"
                fillText(show c, x - 450, 50 + 30 * i)
                restore ()

            flip mapM_ points $ send context . dot adjuster

            send context $ do
              save ()
              translate (-30, 100)
              font "40pt arial"
              fillText("CanvasGraph", 150, 100)

              translate (10, 70)
              font "30pt times"
              fillText("Simple, Cross-Platform Graphing", 150, 100)

              translate (0, 70)
              font "30pt times"
              fillText("From Haskell -> To Your Browser", 150, 100)

              restore ()
