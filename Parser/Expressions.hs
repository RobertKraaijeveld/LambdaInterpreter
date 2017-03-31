module Expressions where

{-
Declaring the possible results (!: meaning ONLY between the curly braces)
of the datatypes.

VERY IMPORTANT: Stuff like Application can be'nullary type constructors'; 
they have no data but their name. Haskell sure is funky.

Any data that comes after them are their construction parameters.

They are declared in the data expressions below.

Also remember that only the right-hand part of 
the grammar rules have to be declared as types. 
-}

type Id = String

data Operator = Add
    | Sub
    | Mul
    deriving (Eq, Show)

data Expression = Body Id Expression
    | Application Expression Expression
    | Var Id
    | Num Int
    | Binop Operator Expression Expression
    deriving (Eq, Show)
    --find out how deriving eq works


toString :: Expression -> String
toString expr = case expr of
    (Body id ex) -> parentheses $ "/" ++ id ++ " -> " ++ toString ex
    (Application ex1 ex2) -> parentheses $ toString ex1 ++ " " ++ toString ex2
    (Binop op ex1 ex2) -> parentheses $ toString ex1 ++ toStringOp op ++ toString ex2
    (Var x) ->  x
    (Num m) ->  show m
    where 
        toStringOp Add = " + "
        toStringOp Mul = " * "
        toStringOp Sub = " - "
        parentheses value = "(" ++ value ++ ")"

data Token = TokenLet
    | TokenIn
    | TokenNum Int
    | TokenVar String
    | TokenEq 
    | TokenLambda
    | TokenArrow     
    | TokenPlus
    | TokenMinus
    | TokenTimes
    | TokenOB 
    | TokenCB 
