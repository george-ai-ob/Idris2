module Compiler.RefC.ProtoIR.Codegen

import Compiler.RefC.ProtoIR.ProtoIR

import System.File.ReadWrite
import Data.Vect

--------------------------------------------------------------------------------
--          ToString Utils
--------------------------------------------------------------------------------

mutual
  ||| Show a list of arguments in a function with spacing and commas
  showArgs : {arity : Nat} -> Vect arity Expression -> String
  showArgs [] = ""
  showArgs {arity=(S len)} (arg :: args) =
    show arg ++ (if len == 0 then "" else ", ") ++ showArgs args

  ||| Show function for Expressions
  ||| Describes how each can be turned into a string of C
  show : Expression -> String
  show (Literal k) = show k
  show (Pred x) = show x ++ " - 1"
  show (Variable str) = str
  show (Function name arity args) = name ++ "(" ++ showArgs args ++ ")"
  show add with (simplify add)
    _ | (Add x y) = show x ++ " + " ++ show y
    _ | x = show x

||| Implementation of Interpolation interface for Expression and Statement
||| to allow interpolated string building and avoid having ++ concatenation
Interpolation Expression where
  interpolate (Literal k) = show k
  interpolate (Variable str) = str
  interpolate (Pred x) = "\{simplify x} - 1"
  interpolate (Function name arity args) = "\{name}(\{showArgs args})"
  interpolate add with (simplify add)
    _ | (Add x y) = "(\{x} + \{y})"
    _ | x = show x

{expression : Expression} -> Interpolation (Statement expression) where
  interpolate (Decl var) = "int \{var};\n"
  interpolate (Assign var value) = "\{var} = \{value};\n"
  interpolate (Return value) = "return \{value};\n"
  interpolate {expression=(Function name arity args)} (FuncDecl name cases) =
    let expr = (Function name arity args) in
      "int \{expr}\n"


--------------------------------------------------------------------------------
--          Pretty Printing
--------------------------------------------------------------------------------

||| insert the types into a list of arguments in a function
mapInt : Vect n Expression -> Vect n Expression
mapInt [] = []
mapInt ((Variable str) :: xs) = (Variable "int \{str}") :: (mapInt xs)
mapInt (x :: xs) = x :: (mapInt xs)

||| repeat a string a specific number of times
replicate : String -> Nat -> String
replicate str 0 = ""
replicate str (S k) = str ++ replicate str k

||| Prefix type can hold a prefix for a block of code (switch, case) and the
||| index of the case
data Prefix : Type where
  MkPrefix : String -> Nat -> Prefix

||| Build a string of C code from a block managing the indentation and
||| translating cases in functions to switch statements and cases
public export
pretty : (tab : Nat) -> (pref : Prefix) -> (terminal : Nat) -> Block -> String
pretty tab pref terminal Empty = ""
pretty tab (MkPrefix pref n) terminal (Next (Left (FuncDecl {arity} {args} name cases)) block)
  with (replicate "\t" tab)
    _ | tabs = let (arg :: tail) = args in

      "\{tabs}\{FuncDecl {arity} {args=(mapInt args)} name cases}\{tabs}{\n\{
        pretty (tab + 1) (MkPrefix "switch (\{arg}) {" n) terminal block}\{
      replicate "\t" (tab + 1)}}\n}"

pretty tab (MkPrefix pref n) terminal (Next (Left x) block) with (replicate "\t" tab)
  _ | tabs =
    "\{tabs}\{x}\{pretty tab (MkPrefix pref n) terminal block}"

pretty tab (MkPrefix pref n) terminal (Next (Right x) block) with (replicate "\t" tab)
  _ | tabs = let nextN = if pref == "" then n + 1 else n in
    "\{tabs}\{

    if pref == "" then
      (if n == terminal then
        "default :"
      else
        "case \{show n}:")
    else
      pref}\n\{

    pretty (tab + 1) (MkPrefix "" n) terminal x}\{
    pretty tab (MkPrefix "" nextN) terminal block}"

||| Write code to specified file
public export
emit : HasIO IO => (fileName : String) -> (content : String) -> IO (Either FileError ())
emit fileName content = do _ <- writeFile fileName ""
                           appendFile fileName content
