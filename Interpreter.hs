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
            -- | paramMatch (Body ids exprs) == True = interpret (Body reducedIdsList newBodyExpr) (tail args)
            | otherwise = interpret (Body ids newBodyExpr) (tail args) --arg gets reduced too
            where 
                newBodyExpr = substArg (Body ids exprs) (head args)
                reducedIdsList = drop 1 ids 

substArg :: Expression -> Expression -> [Expression]
substArg (Body ids exprs) (argExpr) = argExpr : exprs  
                
--this function will break whenever new arguments are added to the left instead of to the right...
--DONT FORGET TO sanitizeBodyParams EVERYWHERE. MAYBE DO THIS AT TOP LEVEL OR IN TOSTRING?
paramMatch :: Expression -> Bool
paramMatch (Body ids exprs) 
            | elem firstId separatedBodyExprVars == True = True   
            | otherwise = False
            where 
                firstId = head (sanitizeBodyParams ids) 
                separatedBodyExprVars = sanitizeBodyParams bodyExprVars
                bodyExprVars = if getExprVars  argVarsExpr /= Nothing then fromJust(getExprVars argVarsExpr) else []
                argVarsExpr = head exprs --this would be the variables after the arrow in a blank lambda.

sanitizeBodyParams :: [String] -> [String]
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
