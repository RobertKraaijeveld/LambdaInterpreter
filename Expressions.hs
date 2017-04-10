module Expressions where

data Operator = Add
    | Sub
    | Mul
    deriving (Eq, Show)

data Expression = Body [String] Expression
    | Application Expression Expression
    | Var [String]
    | Num Int
    | Binop Operator Expression Expression
    deriving (Eq, Show)

--check if these are correct for multiple lambdas
getBody :: Expression -> Maybe Expression
getBody (Body ids expr) = Just (Body ids expr)
getBody (Application expr expr') = getBody(expr)
getBody _ = Nothing

--this might lead to race conditions if both are true
getArgument :: Expression -> Maybe Expression
getArgument (Body ids expr) = Just (expr)
getArgument (Application expr expr') = Just (expr')
getArgument _ = Nothing 

getBodyExprVars :: Expression -> Maybe [String]
getBodyExprVars (Var strList) = Just (strList)
getBodyExprVars (Body ids expr) = getBodyExprVars(expr)
getBodyExprVars _ = Nothing 

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
