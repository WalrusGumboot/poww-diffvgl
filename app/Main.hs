{-# OPTIONS_GHC -Wno-missing-fields #-}

module Main (main) where

import Lib

data Methode = Methode
  { naam :: String,
    fn :: Params -> ((Vct -> Nm) -> Vct -> Int -> Nm)
  }

-- tekenAlles :: Params -> Methode -> IO ()
-- tekenAlles params methode = do
--   putStrLn ("Tekenen voor " ++ naam methode)
--   mapM_
--     ( \μV -> do
--         let oParams = params {μ = μV}
--         putStrLn (" - Voor mu = " ++ show μV)
--         periode <- berekenPeriode oParams (fn methode)
--         putStr "   - periode: "
--         print periode
--         putStrLn "   - x(t)-plot"
--         teken (naam methode ++ "-xt-" ++ show μV) naarDoublesOpl oParams (fn methode) periode
--         putStrLn "   - fasevlak"
--         teken (naam methode ++ "-xy-" ++ show μV) naarDoublesFase oParams (fn methode) periode
--     )
--     [0.2, 1.0, 5.0]

main :: IO ()
main = do
  let params = Params {h = 0.01, initieel = [2, 0], steps = 8000, discard = 200}
  let afwParams = Params {h = 0.01, initieel = [5, 0], steps = 20000, discard = 15000}
  let methodes =
        [ Methode {naam = "RungeKutta", fn = rungeKutta},
          Methode {naam = "VerbeterdeEuler", fn = verbeterdeEuler},
          Methode {naam = "Euler", fn = euler}
        ]
  let afwMethodes =
        [ Methode {naam = "AfwRungeKutta", fn = rungeKutta},
          Methode {naam = "AfwVerbeterdeEuler", fn = verbeterdeEuler},
          Methode {naam = "AfwEuler", fn = euler}
        ]

  -- mapM_ (tekenAlles params) methodes
  -- mapM_ (tekenAlles afwParams) afwMethodes
  mapM_ (\m -> tekenMetMus [0.2, 1.0, 5.0] (naam m ++ "-xt") naarDoublesOpl params (fn m)) methodes
  mapM_ (\m -> tekenMetMus [0.2, 1.0, 5.0] (naam m ++ "-xt") naarDoublesOpl afwParams (fn m)) afwMethodes
  mapM_ (\m -> tekenMetMus [0.2, 1.0, 5.0] (naam m ++ "-xy") naarDoublesFase params (fn m)) methodes
  mapM_ (\m -> tekenMetMus [0.2, 1.0, 5.0] (naam m ++ "-xy") naarDoublesFase afwParams (fn m)) afwMethodes