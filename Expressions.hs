module Expressions where

data Operator = Add
    | Sub
    | Mul
    deriving (Eq, Show)



data Expression = Body [String] [Expression]
    | Application Expression Expression
    | Var [String]
    | Num Int
    | Binop Operator Expression Expression
    deriving (Eq, Show)

--check if these are correct for multiple lambdas
getBody :: Expression -> Maybe Expression
getBody (Body ids exprs) = Just (Body ids exprs)
getBody (Application expr expr') = getBody(expr)
getBody _ = Nothing

--Alternatively, make this alos return on Num?
getExprVars :: Expression -> Maybe [String]
getExprVars (Var strList) = Just (strList)
getExprVars _ = Nothing 

getBodyExpressions :: Expression -> Maybe [Expression]
getBodyExpressions (Body ids exprs) = Just(exprs)
getBodyExpressions _ = Nothing 

--incorrect for multiple arguments ><
getArgument :: Expression -> Maybe Expression
getArgument (Application expr expr') = Just (expr')
getArgument _ = Nothing 


--make array output prettier and remove arrows and shit if there are no body variables
toString :: Expression -> String
toString expr = case expr of
    (Body id ex) -> parentheses $ "/" ++ show id ++ " -> " ++ show ex
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
