module Expressions where

type Id = [String]

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

getBody :: Expression -> Maybe Expression
getBody (Body id expr) = Just (Body id expr)
getBody (Application expr expr') = getBody(expr)
getBody (Application expr expr') = getBody(expr')
getBody _ = Nothing

--make array output prettier
toString :: Expression -> String
toString expr = case expr of
    (Body id ex) -> parentheses $ "/" ++ show id ++ " -> " ++ toString ex
    (Application ex1 ex2) -> parentheses $ toString ex1 ++ " " ++ toString ex2
    (Binop op ex1 ex2) -> parentheses $ toString ex1 ++ toStringOp op ++ toString ex2
    (Var x) ->  show x 
    (Num m) ->  show m
    where 
        toStringOp Add = " + "
        toStringOp Mul = " * "
        toStringOp Sub = " - "
        parentheses value = "(" ++ value ++ ")" 


--ONLY USE THIS AFTER CHECKING THAT OUR MAYBE IS ACTUALLY A JUST
fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "fromJust got a Nothing."
