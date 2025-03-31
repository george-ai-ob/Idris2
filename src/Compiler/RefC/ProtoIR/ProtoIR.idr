module Compiler.RefC.ProtoIR.ProtoIR

import Data.Vect
import Data.Fin

public export
data Expression : Type where
  ||| Constructor for natural number literals
  Literal : (num : Nat) -> Expression
  ||| Constructor to combine two values with addition
  Add : (left : Expression) -> (right : Expression) -> Expression
  ||| Predecessor constructor used to get k in num = S k
  Pred : (num : Expression) -> Expression
  ||| Constructor for named variables
  Variable : (name : String) -> Expression
  ||| Constructor for named functions
  ||| The arity argument refers to one fewer than the total number of arguments
  ||| to ensure that each function has at least 1 argument
  Function : (name : String) -> (arity : Nat) -> (args : Vect (S arity) Expression) -> Expression

public export
data Statement : Expression -> Type where
  ||| Declare a new named Variable
  Decl     : (var : String) -> Statement (Variable var)
  ||| Assign a value to a named Variable
  Assign   : (var : String) -> (value : Expression) -> Statement (Variable var)
  ||| Return a value from a function
  Return   : (value : Expression) -> Statement value
  ||| Declare a new named Function
  FuncDecl : (name : String) -> (cases : Fin n) -> Statement (Function name arity args)

public export
data Block : Type where
  ||| Base case
  Empty : Block
  ||| Create a list of statements organised in blocks
  Next  : {expr : Expression}
       -> (Either (Statement expr) Block) -- each element can either be a single statement or nested block
       -> Block
       -> Block

||| simplify expressions by combining additions where values are known and
||| removing redundant values in addition i.e. 0
public export
simplify : Expression -> Expression
simplify (Literal x) = Literal x
simplify (Add (Literal 0) y) = simplify y
simplify (Add x (Literal 0)) = simplify x
simplify (Add (Literal x) (Literal y)) = Literal (x + y)
simplify (Add x y) with (simplify x) | (simplify y)
  _ | (Literal x') | (Literal y') = Literal (x' + y')
  _ | x' | y' = Add (simplify x') (simplify y')
simplify (Pred x) = Pred x
simplify (Variable str) = Variable str
simplify (Function name arity args) = Function name arity args
