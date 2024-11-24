module FP_lab3

import Data.String
import InfIO

%default total

record Point where
  constructor MkPoint
  x : Double 
  y : Double

Show Point where 
  show (MkPoint x y) = "(" ++ (show x) ++ ";" ++ (show y) ++ ")\n"

getDouble : IO Double
getDouble = InfIO.do line <- getLine 
                     let num = parseDouble line 
                     let parsedNum = case num of 
                                    Nothing => (putStrLn "Ввод не распознан введите еще раз!") >>= (\v => getDouble)
                                    (Just x) => pure x
                     parsedNum

getPoint : IO Point
getPoint = 
  do putStrLn "Введите точку"
     putStr "X: "
     x <- getDouble 
     putStr "Y: "
     y <- getDouble
     pure $ MkPoint x y 

mainInf : InfIO 

main : IO()
main = ?dfk


