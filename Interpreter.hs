module Interpreter where

import Debug.Trace
import Expressions

interpret :: Expression -> [Expression] -> String
interpret expr argList
            | maybeBody == Nothing || null argList || noBodyArgs = toString(expr) 
            | otherwise = reduceBody (fromJust(maybeBody)) argList 
            where 
                maybeBody = getBody expr
                noBodyArgs = noBodyArgsLeft (fromJust(maybeBody))   
 
reduceBody :: Expression -> [Expression] -> String
reduceBody (Body ids exprs) (args) 
            | paramMatch (Body ids exprs) == True = interpret (Body reducedIdsList newBodyExpr) (tail args)
            | otherwise = interpret (Body reducedIdsList exprs) (tail args) --arg gets reduced even if its not moved into body
            where 
                commaSeprtdIds = sanitizeBodyParams ids
                reducedIdsList = tail (sanitizeBodyParams ids) 
                newBodyExpr = substArg (Body reducedIdsList exprs) (head args)
                
substArg :: Expression -> Expression -> [Expression]
substArg (Body reducedIds exprs) (argExpr) 
                                | null exprs = exprs ++ [argExpr]
                                | otherwise =  
                                        let
                                            reducedIdsExpr = Var reducedIds
                                            exprsWithoutIds= tail exprs
                                        in
                                            [reducedIdsExpr] ++ exprsWithoutIds ++ [argExpr]
paramMatch :: Expression -> Bool
paramMatch (Body ids exprs) 
            | firstId == (head separatedBodyExprVars) = True    
            | otherwise = False
            where 
                firstId = head (sanitizeBodyParams ids) 
                separatedBodyExprVars = sanitizeBodyParams bodyExprVars
                bodyExprVars = getVarList argVarsExpr
                argVarsExpr = head exprs --EXPRS IS EMPTY AT FIRST RUN, WHY?

getVarList :: Expression -> [String]
getVarList varsExpr = if getExprVars varsExpr /= Nothing then fromJust(getExprVars varsExpr) else []

sanitizeBodyParams :: [String] -> [String]
sanitizeBodyParams [] = []
sanitizeBodyParams list = splitLambdaParameters (==',') (head list) 

noBodyArgsLeft :: Expression -> Bool
noBodyArgsLeft (Body ids exprs) = null ids                   
                                        
--dropWhile = leave out of returnlist if true. So commas themselves are left out
--break = basically split: break creates a tuple of two lists from the original one separated at condition boundary 
--so we break on a comma, then word becomes whatever is before that comma, str'' becomes the rest.
--we then add word to the returnlist and recurse into str''
splitLambdaParameters :: (Char -> Bool) -> String -> [String]
splitLambdaParameters pred str = case dropWhile pred str of
                                    "" -> []
                                    str' -> word : splitLambdaParameters pred rest
                                        where (word, rest) = break pred str'
