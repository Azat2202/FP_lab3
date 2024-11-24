module InfIO

%default total


public export
data InfIO : Type where
     Do : IO a -> (a -> Inf InfIO) -> InfIO

public export 
(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do 

public export
(>>) : IO a -> (Inf InfIO -> Inf InfIO) 
(>>) x y = y


public export
data Fuel = More (Lazy Fuel)

partial
public export
forever : Fuel
forever = More forever

public export
run : Fuel -> InfIO -> IO ()
run (More fuel) (Do c f) = 
  do res <- c 
     run fuel (f res)

interpolation : InfIO 
interpolation = 
  InfIO.do 
    putStrLn "What is your name?"
    name <- getLine
    interpolation
    -- putStrLn ("Hello, " ++ name)



