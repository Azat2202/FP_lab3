module FP_lab3

import Data.String
import Data.Fuel
import Data.Vect
import System

%default total

record Point where
  constructor MkPoint
  x : Double 
  y : Double

zPoint : Point
zPoint = MkPoint 0.0 0.0 

Show Point where 
  show (MkPoint x y) = (show x) ++ "\t" ++ (show y)

has : String -> List String -> Bool
has str strs = case find (== str) strs of
                    Nothing => False
                    (Just x) => True

partial
parseDiscretisation : List String -> Double
parseDiscretisation strs = case find (isPrefixOf "-d") strs of
                                Nothing => 1.0
                                (Just arg) => let (d, discretisation) = span (/= '=') arg in 
                                                  case parseDouble (strTail discretisation) of
                                                       Nothing => 1.0
                                                       (Just x) => x

getPoint : IO Point
getPoint = do putStrLn "Введите точку в формате (x y) (q чтобы выйти)"
              line <- getLine 
              _ <- if (line == "q") then exitSuccess else pure ()
              let (xStr, yStr) = span (/= ' ') line
              let x = fromMaybe 0.0 $ parseDouble xStr
              let y = fromMaybe 0.0 $ parseDouble yStr
              pure $ MkPoint x y

unzipPoint : Point -> (Double, Double)
unzipPoint (MkPoint x y) = (x, y)

lagrange_l : {len: Nat} -> (n: Fin (S (S len))) -> Double -> Vect (S (S len)) Double -> Double
lagrange_l i x xs = numerator / denominator where
                      numerator : Double
                      numerator = foldl1 (*) $ deleteAt i $ zipWith (-) (replicate (S (S len)) x) xs

                      denominator : Double
                      denominator = let x_i = index i xs in
                        foldl1 (*) $ deleteAt i $ zipWith (-) (replicate (S (S len)) x_i) xs

lagrange_L : {len : Nat} -> Double -> Vect (S (S len)) Point -> Point
lagrange_L x points = let (xs, ys) = unzip $ map unzipPoint points
                          indexes = Data.Vect.Fin.range {len=S(S len)}
                          l_i = map (\i => lagrange_l i x xs) indexes 
                          y = foldl1 (+) $ zipWith (*) ys l_i in 
                          MkPoint x y


doubleRange : (start: Double) -> (end: Double) -> (discretisation: Double) -> List Double
doubleRange start end discretisation = case start < end of
                                            False => [start]
                                            True => assert_total $ start :: (doubleRange (start + discretisation) end discretisation)

lagrange : Vect 4 Point -> (discretisation : Double) -> IO ()
lagrange xs@(p0@(MkPoint x0 y0) :: p1 :: p2 :: p3@(MkPoint x3 y3) :: Nil) discretisation = 
  do _ <- putStrLn "\nИнтерполяция Лагранжа"
     let range = doubleRange x0 x3 discretisation
     let points = map (\x => lagrange_L x xs) range
     putStrLn $ foldl1 (++) $ [""] ++ map ((++ "\n") . show) points

linearFunc : Vect 2 Point -> Double -> Point
linearFunc xs@((MkPoint x0 y0) :: ((MkPoint x1 y1) :: [])) x = 
  MkPoint x $ y0 + (x - x0) * (y1 - y0) / (x1 - x0)

linear : Vect 2 Point -> (discretisation : Double) -> IO ()
linear xs@((MkPoint x0 y0) :: ((MkPoint x1 y1) :: [])) discretisation = 
  do _ <- putStrLn "\nЛинейная интерполяция"
     let range = doubleRange x0 x1 discretisation
     let points = map (linearFunc xs) range 
     putStrLn $ foldl1 (++) $ [""] ++ map ((++ "\n") . show) points

interpolationGo : Fuel -> 
                  Vect 3 Point -> 
                  (isLinear : Bool) -> 
                  (isLagrange : Bool) ->
                  (discretisation : Double) ->
                  IO ()
interpolationGo Dry _ _ _ _ = pure ()
interpolationGo (More fuel) (p0 :: p1 :: p2 ::  Nil) isLinear isLagrange discretisation = 
  do putStrLn "Введите следующую точку"
     p3 <- getPoint
     if isLinear then (do linear [p2, p3] discretisation)
                 else pure ()
     if isLagrange then (do lagrange [p0, p1, p2, p3] discretisation)
                   else pure ()
     interpolationGo fuel [p1, p2, p3] isLinear isLagrange discretisation

record InterpolationState where
  constructor MkState
  interpolators : List (Vect l Point -> IO ())

data InterpolationCmd : Type -> Nat -> Nat -> Type where 
  TwoPoints :   (p1 : Point) -> (p2: Point) -> InterpolationCmd   InterpolationState  0 2
  ThreePoints : (p3 : Point) ->                InterpolationCmd   InterpolationState  2 3
  FourPoints :  (p4 : Point) ->                InterpolationCmd   InterpolationState  3 4
  InfPoints :   (p5 : Point) ->                InterpolationCmd   InterpolationState  4 4

  Pure : ty -> InterpolationCmd ty nat nat
  (>>=) : InterpolationCmd s1 a1 a2 -> (s1 -> InterpolationCmd s2 a2 a3) -> InterpolationCmd s2 a1 a3


interpolationLoop : Fuel -> {s1: Nat} -> InterpolationCmd () s1 s2
interpolationLoop Dry = ?slkfj
interpolationLoop (More fuel) {s1 = 0} = do let p1 = zPoint
                                            (MkState interpolators) <- TwoPoints p1 p1
                                            _ <- ThreePoints p1
                                            ?dkfj
interpolationLoop (More fuel) {s1 = 1} = ?asdklfh
interpolationLoop (More fuel) {s1 = 2} = ?interpolationLoop_rhs_4
interpolationLoop (More fuel) {s1 = _} = ?interpolationLoop_rhs_5


partial
main : IO()
main = do putStrLn ""
          args <- getArgs
          let isLinear = has "linear" args
          let isLagrange = has "lagrange" args
          let discretisation = parseDiscretisation args

          putStrLn "Введите первые две точки"
          point1 <- getPoint
          point2 <- getPoint 
          if isLinear then (do linear [point1, point2] discretisation)
                      else pure ()

          putStrLn "Введите следующую точку"
          point3 <- getPoint
          if isLinear then (do linear [point2, point3] discretisation)
                      else pure ()

          interpolationGo forever [point1, point2, point3] isLinear isLagrange discretisation
