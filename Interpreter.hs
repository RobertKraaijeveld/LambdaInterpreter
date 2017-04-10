module Interpreter where

import Expressions

interpret :: Expression -> String
interpret expr 
            | maybeBody == Nothing || maybeArg == Nothing = toString(expr) 
            | otherwise = reduceBody (fromJust(maybeBody)) (fromJust(maybeArg)) 
            where 
                maybeBody = getBody expr
                maybeArg = getArgument expr

reduceBody :: Expression -> Expression -> String
reduceBody (Body ids exp) (argExpr)
            | paramMatch (Body ids exp) == True = interpret (Body reducedIdsList argExpr)
            | otherwise = interpret (Body reducedIdsList exp) -- second expression removed entirely 
            where 
                reducedIdsList = tail ids 

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
                                        
--dropWhile = leave out of returnlist if true. So commas themselves are left out
--break = basically split: break creates a tuple of two lists from the original one separated at condition boundary 
--so we break on a comma, then word becomes whatever is before that comma, str'' becomes the rest.
--we then add word to the returnlist and recurse into str''
splitLambdaParameters :: (Char -> Bool) -> String -> [String]
splitLambdaParameters pred str = case dropWhile pred str of
                                    "" -> []
                                    str' -> word : splitLambdaParameters pred rest
                                        where (word, rest) = break pred str'
