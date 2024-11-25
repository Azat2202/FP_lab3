module InfIO

%default total

public export
data RunIO : Type -> Type where
        Quit : a -> RunIO a
        Do : IO a -> (a -> Inf (RunIO b)) -> RunIO b

public export
(>>=) : IO a -> (a -> Inf (RunIO b)) -> RunIO b
(>>=) = Do 

-- not total lol)
--(>>) : IO () -> Inf (RunIO Inf (b)) -> RunIO b
--a >> b = a >>= (\_ => b)

public export
greet : RunIO ()
greet = InfIO.do _ <- putStr "Enter your name: "
                 name <- getLine
                 if name == ""
                    then do _ <- putStrLn "Bye bye!"
                            Quit ()
                    else do _ <- putStrLn ("Hello " ++ name)
                            greet

public export
data Fuel = Dry | More (Lazy Fuel)

public export
partial 
forever : Fuel
forever = More forever

public export
run : Fuel -> RunIO a -> IO (Maybe a)
run fuel (Quit value) = pure (Just value)
run (More fuel) (Do c f) = do res <- c
                              run fuel (f res)
run Dry p = pure Nothing

partial
main : IO ()
main = do _ <- run forever greet
          pure ()
