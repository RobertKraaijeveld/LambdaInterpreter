module Interpreter where

import Expressions

--  if (getBody parsedInput) == Nothing then print "nothing" else print $ toString (fromJust (getBody x))
-- case expression of pattern -> result  
interpret :: Expression -> String
interpret expr 
            | maybeBody == Nothing || maybeArg == Nothing = toString(expr) 
            | otherwise = parseBody (fromJust(getBody expr))
            where 
                maybeBody = getBody expr
                maybeArg = getArgument expr

parseBody :: Expression -> String
parseBody body 
            -- go on
            | paramMatches body == True = toString(body)
            | otherwise = "other"
            where
                paramMatches = paramMatch 

--DONT FORGET TO sanitizeBodyParams EVERYWHERE. MAYBE DO THIS AT TOP LEVEL OR IN TOSTRING?
paramMatch :: Expression -> Bool
paramMatch (Body ids exp) 
            | elem firstId separatedBodyExprVars == True = True   
            | otherwise = False
            where 
                -- make second where cleaner. Also sanitize body expr vars
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
