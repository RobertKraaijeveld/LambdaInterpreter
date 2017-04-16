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

anyBodies :: Expression -> Bool
anyBodies expr 
            | maybeBody /= Nothing = True
            | otherwise = False
            where
                maybeBody = getBody expr

getBodies :: [Expression] -> [Expression]
getBodies args = 
                let
                    nonDoubleArgs = filter(\x -> getBody x /= Nothing) args 
                in
                    map (\x -> fromJust(getBody x)) nonDoubleArgs

getArgs :: Expression -> [Expression]
getArgs (Application expr expr') = getArgs expr ++ getArgs expr'
getArgs (Body ids exprs) = [] --not necessary to add as arg: each body is done individually and then they are glued together using combineBodies :)
getArgs (Binop op expr1 expr2) = [(Binop op expr1 expr2)]
getArgs (Var strList) = [(Var strList)]
getArgs (Num n) = [(Num n)]
getArgs _ = [] 


getMultiArgs :: [Expression] -> [Expression] 
getMultiArgs [] = []
getMultiArgs exprList = getMultiArgs (tail exprList) ++ getArgs (head exprList)

getBody :: Expression -> Maybe Expression
getBody (Body ids exprs) = Just (Body ids exprs)
getBody (Application expr expr') = getBody(expr)
getBody _ = Nothing

--this is used in the (xyz) BEFORE the arrow
getBodyIds :: Expression -> [String]
getBodyIds (Body ids exprs) = splitListByComma ids


--whilst this is used in the (xyz) AFTER the arrow
getExprVars :: Expression -> Maybe [String]
getExprVars (Var strList) = Just (strList)
getExprVars _ = Nothing 

getBodyExpressions :: Expression -> Maybe [Expression]
getBodyExpressions (Body ids exprs) = Just(exprs)
getBodyExpressions _ = Nothing 

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


{-
  GLUEING ALL INTERPRETED BODIES TOGETHER
-}

collectAllBodies :: Expression -> [Expression] -> [Expression]
collectAllBodies rootExpr args
                        | anyBodies rootExpr == True = [fromJust(maybeInitialBody)] ++ getBodies args
                        | otherwise = []
                        where 
                          maybeInitialBody = getBody rootExpr

combineBodies :: [Expression] -> String
combineBodies [] = "Empty"
combineBodies allExprList = 
                      let
                        rootExpr = [head allExprList] 
                        rootExprBodies = getBodies rootExpr
                        interpretedBodies = getBodies (tail allExprList)
                      in
                        show (bodyExprsToStr (fromJust(getBodyExpressions (head rootExpr))) interpretedBodies)

fullToString :: Expression -> [Expression] -> [String]
fullToString expr interpretedBodies = case expr of
              (Body ids exprs) -> [toString (head interpretedBodies)] 
              (Application expr expr') -> fullToString expr interpretedBodies
              _  -> [toString expr]

bodyExprsToStr :: [Expression] -> [Expression] -> [String]
bodyExprsToStr [] _ = []
bodyExprsToStr bodyExprList interpretedBodies = fullToString (head bodyExprList) interpretedBodies ++ bodyExprsToStr (tail bodyExprList) interpretedBodies  


{-
TO STRING
-}                            


--make array output prettier and remove arrows etc there are no body variables
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

{-
UTILITIES
-}                            

--calling show on empty list returns braces, Make it return nothing at all
varExprToStr :: [Expression] -> String
varExprToStr exprs 
            | null ids = ""
            | otherwise = show ids
            where
                ids = fromJust(getExprVars idsExpr)
                idsExpr = head exprs

--WARNING: ONLY USE THIS AFTER CHECKING THAT A MAYBE IS ACTUALLY A JUST
fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "fromJust got a Nothing."


--the extra conditions make sure that an already sane list, like ["x", "y"] is not re-done.
splitListByComma :: [String] -> [String]
splitListByComma [] = []
splitListByComma list 
                        | length list == 1 = splitFilter (==',') (head list) 
                        | otherwise = list

noBodyArgsLeft :: Expression -> Bool
noBodyArgsLeft (Body ids exprs) = null ids                   
                                        
--dropWhile = leave out of returnlist if true. So commas themselves are left out
--break = basically split: break creates a tuple of two lists from the original one separated at condition boundary 
--so we break on a comma, then word becomes whatever is before that comma, str'' becomes the rest.
--we then add word to the returnlist and recurse into str''
splitFilter :: (Char -> Bool) -> String -> [String]
splitFilter pred str = case dropWhile pred str of
                                    "" -> []
                                    str' -> word : splitFilter pred rest
                                        where (word, rest) = break pred str'
