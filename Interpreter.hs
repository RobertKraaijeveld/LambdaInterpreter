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
 
-- paramMatch doesnt use the sanitizeBodyParams so it fails early
reduceBody :: Expression -> [Expression] -> String
reduceBody (Body ids exprs) (args) 
            | paramMatch (Body (sanitizeBodyParams ids) exprs) == True = interpret (Body reducedIdsList newBodyExpr) (tail args)
            | otherwise = interpret (Body reducedIdsList exprs) (tail args) --arg gets reduced too
            where 
                newBodyExpr = substArg (Body ids exprs) (head args)
                reducedIdsList = tail (sanitizeBodyParams ids) 

--also make this remove the head of the rightmost Expressions' stringlist 
substArg :: Expression -> Expression -> [Expression]
substArg (Body ids exprs) (argExpr) = argExpr : exprs  
                
paramMatch :: Expression -> Bool
paramMatch (Body ids exprs) 
            | null separatedBodyExprVars || null (sanitizeBodyParams ids) = False
            | firstId == (head separatedBodyExprVars) = True   
            | otherwise = False
            where 
                firstId = head (sanitizeBodyParams ids) 
                separatedBodyExprVars = sanitizeBodyParams bodyExprVars
                bodyExprVars = if getExprVars argVarsExpr /= Nothing then fromJust(getExprVars argVarsExpr) else []
                argVarsExpr = head exprs --this would be the variables after the arrow in a blank lambda.

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
