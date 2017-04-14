module Interpreter where

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
                reducedIdsList = tail commaSeprtdIds
                commaSeprtdIds = sanitizeBodyParams ids                
                newBodyExpr = substArg (Body reducedIdsList exprs) (head args) (ids)


substArg :: Expression -> Expression -> [String] -> [Expression]
substArg (Body reducedIds exprs) (argExpr) unmodifiedIds 
                                            | null exprs = exprs ++ [argExpr]
                                            | otherwise = [newIdsExpr] ++ exprsWithoutIdExpr ++ [argExpr]
                                                where
                                                    newIdsExpr = reduceIdsExpr unmodifiedIds oldIdsExpr 
                                                    oldIdsExpr = head exprs 
                                                    exprsWithoutIdExpr = tail exprs
                                                        



--is order dependant :(
--give these better names
reduceIdsExpr :: [String] -> Expression -> Expression
reduceIdsExpr unmodifiedIds idsExpr =
                                let
                                    reducedIdsResult = filter (\x -> (head (sanitizeBodyParams unmodifiedIds) == x) == False) bodyIdsList 
                                    bodyIdsList = sanitizeBodyParams(fromJust(getExprVars idsExpr)) --unsafe. this returns 1 var too little
                                in
                                    Var combined


paramMatch :: Expression -> Bool
paramMatch (Body ids exprs) 
            | elem firstId separatedBodyExprVars = True    
            | otherwise = False
            where 
                firstId = head (sanitizeBodyParams ids) 
                separatedBodyExprVars = sanitizeBodyParams bodyExprVars
                bodyExprVars = getVarList argVarsExpr
                argVarsExpr = head exprs 

getVarList :: Expression -> [String]
getVarList varsExpr = if getExprVars varsExpr /= Nothing then fromJust(getExprVars varsExpr) else []

--the extra conditions make sure that an already sane list, like ["x", "y"] is not re-done.
sanitizeBodyParams :: [String] -> [String]
sanitizeBodyParams [] = []
sanitizeBodyParams list 
                        | length list == 1 = splitLambdaParameters (==',') (head list) 
                        | otherwise = list

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
