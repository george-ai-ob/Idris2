mul : Nat -> Nat -> Nat
mul 0 y = 0
mul (S x) y = y + mul x y

main : IO ()
main = putStrLn (show (mul 3 5))
