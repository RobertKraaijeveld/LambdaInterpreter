module Expressions where

import qualified Data.Map as Map
import Debug.Trace

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

anyBodies :: Expression -> Bool
anyBodies expr 
            | maybeBody /= Nothing = True
            | otherwise = False
            where
                maybeBody = getBody expr

getBodiesFromArgs :: [Expression] -> [Expression]
getBodiesFromArgs args = 
                let
                    nonDoubleArgs = filter(\x -> getBody x /= Nothing) args --this just filters exprs that CONTAIN bodies; we want the bodies themselves.
                in
                    map (\x -> fromJust(getBody x)) nonDoubleArgs

--include nullcheck cunt
getArgs :: Expression -> [Expression]
getArgs (Application expr expr') = getArgs (fromJust(getAppArg1 (Application expr expr'))) ++ [fromJust(getAppArg2 (Application expr expr'))] --UGLY, NONCHECKED FROMJUSTS REEEE
getArgs (Body ids exprs) = getMultiArgs exprs
getArgs _ = []

getMultiArgs :: [Expression] -> [Expression] 
getMultiArgs [] = []
getMultiArgs exprList = getMultiArgs (tail exprList) ++ getArgs (head exprList)

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

--make these cleaner OH MY LAWD ITS HORRIBLE
getApplication :: Expression -> Maybe Expression
getApplication (Application expr expr') = Just (Application expr expr')
getApplication _ = Nothing 

getAppArg1 :: Expression -> Maybe Expression
getAppArg1 (Application expr expr') = Just (expr)
getAppArg1 _ = Nothing 

getAppArg2 :: Expression -> Maybe Expression
getAppArg2 (Application expr expr') = Just (expr')
getAppArg2 _ = Nothing 

getVarList :: Expression -> [String]
getVarList varsExpr = if getExprVars varsExpr /= Nothing then fromJust(getExprVars varsExpr) else []

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
