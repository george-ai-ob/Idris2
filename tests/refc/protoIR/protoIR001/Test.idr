import ProtoIR
import Codegen
import Data.Fin
import Data.Vect

-- function to compile
mul : Nat -> Nat -> Nat
mul 0 y = 0
mul (S x) y = y + mul x y

-- Intermediate representation of function
mulIR : Block
mulIR = Next (Left (FuncDecl {arity=1} {args=[Variable "x", Variable "y"]} "mul" (FS (FS (FZ {k=0})))))
      (Next {expr=(Variable "x")} (Right
             (Next {expr=(Variable "x")} (Right
                   (Next (Left (Return ((Add (Add (Literal 0) (Literal 0)) (Literal 0)))))Empty))
             (Next {expr=(Variable "x")} (Right
                   (Next (Left (Decl "k"))
                   (Next (Left (Assign "k" (Pred (Variable "x"))))
                   (Next (Left (Return (Add (Variable "y") (Function "mul" 1 [Variable "k", Variable "y"])))) Empty))))
             Empty)))
       Empty)

main : IO ()
main = putStrLn (pretty 0 (MkPrefix "" 0) 1 mulIR)
