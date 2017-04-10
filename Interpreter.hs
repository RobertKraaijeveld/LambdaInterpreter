module Interpreter where

import Debug.Trace
import Expressions

--think maybeArg or maybeBody might cause the early stoppage
--alternative is seeing when the ids[] are empty (also, we have to move on to the next body afterwards.)
interpret :: Expression -> String
interpret expr 
            | maybeBody == Nothing || noBodyArgs == True = toString(expr) 
            | otherwise = reduceBody (fromJust(maybeBody)) (fromJust(maybeArg)) 
            where 
                maybeBody = getBody expr
                maybeArg = getArgument expr 
                noBodyArgs = noBodyArgsLeft (fromJust(maybeBody))   

reduceBody :: Expression -> Expression -> String
reduceBody (Body ids exp) (argExpr) --
            | paramMatch (Body ids exp) == True = interpret (Body reducedIdsList newBodyExpr)
            | otherwise = interpret (Body reducedIdsList exp) --second expression not looked at anymore 
            where 
                newBodyExpr = substArg (Body ids exp) (argExpr)
                reducedIdsList = drop 1 ids 

--WHY DOES maybeMultiExpressionList RETURN NOTHING???
substArg :: Expression -> Expression -> Expression
substArg (Body ids exp) (argExpr) 
            | maybeMultiExpressionList == Nothing = MultiExpression [exp, argExpr]
            | otherwise = MultiExpression combined 
            where 
                maybeMultiExpressionList = getMultiExpressionList exp
                combined = argExpr : fromJust(maybeMultiExpressionList)  
                
--DONT FORGET TO sanitizeBodyParams EVERYWHERE. MAYBE DO THIS AT TOP LEVEL OR IN TOSTRING?
--main.hs: Prelude.head: empty list
paramMatch :: Expression -> Bool
paramMatch (Body ids exp) 
            | elem firstId separatedBodyExprVars == True = True   
            | otherwise = False
            where 
                firstId = head (sanitizeBodyParams ids) 
                separatedBodyExprVars = sanitizeBodyParams bodyExprVars
                bodyExprVars = if getBodyExprVars exp /= Nothing then fromJust(getBodyExprVars exp) else [] 

sanitizeBodyParams :: [String] -> [String]
sanitizeBodyParams list = splitLambdaParameters (==',') (head list)     

noBodyArgsLeft :: Expression -> Bool
noBodyArgsLeft (Body ids exp) = null ids                   
                                        
--dropWhile = leave out of returnlist if true. So commas themselves are left out
--break = basically split: break creates a tuple of two lists from the original one separated at condition boundary 
--so we break on a comma, then word becomes whatever is before that comma, str'' becomes the rest.
--we then add word to the returnlist and recurse into str''
splitLambdaParameters :: (Char -> Bool) -> String -> [String]
splitLambdaParameters pred str = case dropWhile pred str of
                                    "" -> []
                                    str' -> word : splitLambdaParameters pred rest
                                        where (word, rest) = break pred str'
