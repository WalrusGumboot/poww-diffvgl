{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Lib
  ( Params (..),
    Nm,
    Vct,
    euler,
    verbeterdeEuler,
    rungeKutta,
    stap,
    naarDoublesFase,
    naarDoublesOpl,
    teken,
    berekenPeriode,
  )
where

import Data.List (findIndices, nubBy)
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import Graphics.Rendering.Chart.Easy

type Nm = Double

type Vct = [Nm]

data Params = Params {h :: Nm, μ :: Nm, initieel :: Vct, steps :: Int, discard :: Int}

euler :: Params -> (Vct -> Nm) -> Vct -> Int -> Nm
euler params f prev idx = prev !! idx + h params * f prev

verbeterdeEuler :: Params -> (Vct -> Nm) -> Vct -> Int -> Nm
verbeterdeEuler params f prev idx =
  let hV = h params
      k_1 = f prev
      k_2 = f [hV + head prev, hV * k_1 + prev !! 1]
   in prev !! idx + hV * (k_1 + k_2) / 2

rungeKutta :: Params -> (Vct -> Nm) -> Vct -> Int -> Nm
rungeKutta params f prev idx =
  let hV = h params
      k_1 = f prev
      k_2 = f [head prev + hV / 2, prev !! 1 + hV * k_1 / 2]
      k_3 = f [head prev + hV / 2, prev !! 1 + hV * k_2 / 2]
      k_4 = f [head prev + hV, prev !! 1 + hV * k_3]
   in prev !! idx + hV / 6 * (k_1 + 2 * k_2 + 2 * k_3 + k_4)

x' :: Params -> Vct -> Nm
x' _ [_, y] = y

y' :: Params -> Vct -> Nm
y' Params {μ = mu} [x, y] = -x + mu * (1 - x * x) * y

stap :: Params -> ((Vct -> Nm) -> Vct -> Int -> Nm) -> (Vct -> Vct)
stap params f v =
  [ f (x' params) v 0,
    f (y' params) v 1
  ]

type Signaal = Params -> Int -> (Vct -> Vct) -> Vct -> [(Double, Double)]

naarDoublesFase :: Signaal
naarDoublesFase _ n f v = map (\[x, y] -> (x, y)) (take n (iterate f v))

naarDoublesOpl :: Signaal
naarDoublesOpl params n f v = zipWith (curry (\(t, [x, _]) -> (fromIntegral t * h params, x))) [0 .. n] (take n (iterate f v))

teken :: String -> Signaal -> Params -> (Params -> (Vct -> Nm) -> Vct -> Int -> Nm) -> Double -> IO ()
teken naam signaal params f periode = toFile def ("grafieken/" ++ naam ++ ".png") $ do
  let signaalF = signaal params (steps params) (stap params (f params)) (initieel params)
  layout_title .= naam ++ "(mu: " ++ (show . μ) params ++ ", periode: " ++ show periode ++ ")"
  setColors [opaque blue]
  plot (line "signaalF" [signaalF])

berekenPeriode :: Params -> (Params -> ((Vct -> Nm) -> Vct -> Int -> Nm)) -> IO Double
berekenPeriode params f = do
  let signaalF = naarDoublesOpl params (steps params) (stap params (f params)) (initieel params)
  let matches = minima params (drop (discard params) $ map snd signaalF) 1.0
  let accuracy = [fromIntegral a / fromIntegral (head matches) | a <- matches] :: [Double]
  print $ take 4 accuracy
  -- we nemen aan dat onze periode accuraat is, dit blijkt empirisch ook zo te zijn
  return $ fromIntegral (head matches) * h params

afwNaShift :: [Double] -> Int -> Double
afwNaShift xs s = sum $ zipWith (\a b -> (a - b) ** 2) xs (drop s xs)

filterRange :: Int -> [Int] -> [Int]
filterRange limit = nubBy (\a b -> b - a <= limit)

minima :: Params -> [Double] -> Double -> [Int]
minima params xs eps = filterRange 50 $ findIndices (< eps) (map (afwNaShift xs) [1 .. (steps params - discard params)])
