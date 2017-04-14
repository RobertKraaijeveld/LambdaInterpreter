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

--this is ugly. also, test this first.
getArgs :: Expression -> [Expression]
getArgs expr
            | maybeAppExpr /= Nothing = getArgs (fromJust(getArg1 (realAppExpr))) ++ [fromJust(getArg2 (realAppExpr))]
            | otherwise = []
            where 
                realAppExpr = fromJust(maybeAppExpr) --lazy evaluation ftw
                maybeAppExpr = getApplication expr

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

--make these cleaner
getApplication :: Expression -> Maybe Expression
getApplication (Application expr expr') = Just (Application expr expr')
getApplication _ = Nothing 

getArg1 :: Expression -> Maybe Expression
getArg1 (Application expr expr') = Just (expr)
getArg1 _ = Nothing 

getArg2 :: Expression -> Maybe Expression
getArg2 (Application expr expr') = Just (expr')
getArg2 _ = Nothing 


--make array output prettier and remove arrows and shit if there are no body variables
toString :: Expression -> String
toString expr = case expr of
    (Body id exprs) -> parentheses $ bodyToStr expr
    (Application ex1 ex2) -> parentheses $ toString ex1 ++ " " ++ toString ex2
    (Binop op ex1 ex2) -> parentheses $ toString ex1 ++ toStringOp op ++ toString ex2
    (Var x) ->  show x 
    (Num m) ->  show m
    where 
        toStringOp Add = " + "
        toStringOp Mul = " * "
        toStringOp Sub = " - "
        parentheses value = "(" ++ value ++ ")" 

--prettify this more
bodyToStr :: Expression -> String
bodyToStr (Body ids exprs) 
                        | null ids = varExprToStr exprs ++ show (tail exprs)
                        | otherwise = "/" ++ show ids ++ " -> " ++ varExprToStr exprs ++ show (tail exprs)
                            

--calling show on empty list returns braces, Make it return nothing at all
--will break if var insertion order is changed
varExprToStr :: [Expression] -> String
varExprToStr exprs 
            | null ids = ""
            | otherwise = show ids
            where
                ids = fromJust(getExprVars idsExpr)
                idsExpr = head exprs

--ONLY USE THIS AFTER CHECKING THAT OUR MAYBE IS ACTUALLY A JUST
fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "fromJust got a Nothing."
