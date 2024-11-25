module FP_lab3

import Data.String
import InfIO

%default total

record Point where
  constructor MkPoint
  x : Double 
  y : Double

zPoint : Point
zPoint = MkPoint 0.0 0.0 

Show Point where 
  show (MkPoint x y) = "(" ++ (show x) ++ ";" ++ (show y) ++ ")\n"

getDouble : RunIO Double
getDouble = InfIO.do line <- getLine 
                     let num = parseDouble line 
                     let parsedNum = case num of 
                                    Nothing => (putStrLn "Ввод не распознан введите еще раз!") >>= (\v => getDouble)
                                    (Just x) => Quit x
                     parsedNum

getPoint : IO Point
getPoint = 
  do putStrLn "Введите точку"
     _ <- putStr "X: "
     x <- do run (More Dry) getDouble 
     _ <- putStr "Y: "
     y <- do run (More Dry) getDouble
     case x of
          Nothing => pure zPoint -- impossible case actually
          (Just justX) => case y of
                           Nothing => pure zPoint -- impossible case
                           (Just justW) => pure $ MkPoint justX justW

main : IO()
main = do point <- getPoint
          putStrLn ("You wrote "++ (show point))
          pure ()
