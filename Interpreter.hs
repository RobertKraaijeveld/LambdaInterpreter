module Interpreter where

import qualified Data.Map as Map
import Debug.Trace
import Expressions

{-
get all bodies
start with first until either arglist is full or body is done.
do same for next body.
add all tostr representations to a list of strs
out that list
done, only needs prettyfying 
-}

--het doorsturen van args gaat fout, loopen naar volgend body gaat wel prima.
--args ziet namelijk de gehele 2e lambda als 1 arg.
--solution: args per body vinden in interpretBody
--maar dit kan dubbels geven met reeds bestaande arglist.
--DUS: fullArgs = bestaande args ++ getArgs van expr die niet ook in bestaande args zitten

interpret :: [Expression] -> [Expression] -> [String]
interpret allBodies argList
                    | null allBodies || null argList = trace("interpret arglist or bodieslist is empty \n") []
                    | otherwise = trace("BodyList is " ++ show allBodies ++ " sending body " ++ show (head allBodies) ++ " with args " ++ show argList) interpret (tail allBodies) argList ++ [(interpretBody (head allBodies) argList)] 

interpretBody :: Expression -> [Expression] -> String
interpretBody expr argList
            | maybeBody == Nothing || null argsWithoutSelf || noBodyArgs = trace("got some sort of null. maybeBody == " ++ show maybeBody ++ "\n argsWithoutSelf = " ++ show argsWithoutSelf ++ "\n noBodyArgs = " ++ show noBodyArgs)       show expr 
            | otherwise = trace("Interpreting body " ++ show expr ++ " with args " ++ show (getMultiArgs argList) ++ "\n") reduceBody (fromJust(maybeBody)) (argsWithoutSelf)
            where 
                argsWithoutSelf = getMultiArgs argList ++ argList --make into a function call, or otherwise explain why this makes sense and works...
                maybeBody = getBody expr
                noBodyArgs = noBodyArgsLeft (fromJust(maybeBody))   
 
reduceBody :: Expression -> [Expression] -> String
reduceBody (Body ids exprs) (args) 
            | paramMatch (Body ids exprs) == True = trace("Got parammatch. Body is " ++ show (Body ids exprs) ++ "\n") interpretBody (Body reducedIdsList newBodyExpr) (tail args)
            | otherwise = trace("Got no parammatch. Body is " ++ show (Body ids exprs) ++ "\n") interpretBody (Body reducedIdsList exprs) (tail args) --arg gets reduced even if its not moved into body
            where 
                reducedIdsList = tail commaSeprtdIds 
                commaSeprtdIds = sanitizeBodyParams ids --                
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
                                    Var reducedIdsResult

paramMatch :: Expression -> Bool
paramMatch (Body ids exprs) 
            | elem firstId separatedBodyExprVars = True    
            | otherwise = False
            where 
                firstId = head (sanitizeBodyParams ids) 
                separatedBodyExprVars = sanitizeBodyParams bodyExprVars
                bodyExprVars = getVarList argVarsExpr
                argVarsExpr = head exprs 

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
