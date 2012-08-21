module Graph where

import Graphics.Blank
import Control.Arrow hiding (loop)
import System.Random
import GHC.Float
import LinReg

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

main :: IO ()
main = do points <- fmap (take 18) $ annealedData 10 [0,8,-3,1,0,-0.1]

          blankCanvas 5001 $ \ context -> do

            screenSize <- send context size

            let adjuster = adjust screenSize (concat $ points : fits)
                fits     = flip map [2..8] $ \x -> map (id &&& poly (lsrf x points)) (map fst points)

            flip mapM_ fits $ \fit -> do
              send context $ do
                strokeStyle "#EE6666"
                sketch adjuster fit
                lineWidth 4
                stroke

            flip mapM_ points $ send context . dot adjuster
