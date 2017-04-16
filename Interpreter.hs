module Interpreter where

import Expressions

interpret allBodies argList
                    | null allBodies || null argList = []
                    | otherwise = interpret (tail allBodies) argListForCurrBody ++ [(interpretBody currBody argListForCurrBody)] 
                    where
                        argListForCurrBody = take ((length argVarsForCurrBody)) (argList) 
                        argVarsForCurrBody = getBodyIds currBody
                        currBody = head allBodies

interpretBody :: Expression -> [Expression] -> Expression
interpretBody expr argList
            | maybeBody == Nothing || null argsWithoutSelf || noBodyArgs = expr 
            | otherwise = reduceBody (fromJust(maybeBody)) (argsWithoutSelf)
            where 
                argsWithoutSelf = argList ++ getMultiArgs argList --todo: explain why this is correct :P
                maybeBody = getBody expr
                noBodyArgs = noBodyArgsLeft (fromJust(maybeBody))   
 
reduceBody :: Expression -> [Expression] -> Expression
reduceBody (Body ids exprs) (args) 
            | paramMatch (Body ids exprs) == True = interpretBody (Body reducedIdsList newBodyExpr) (tail args)
            | otherwise = interpretBody (Body reducedIdsList exprs) (tail args) --Note that an arg gets reduced even if its not moved into body
            where 
                reducedIdsList = tail commaSeprtdIds 
                commaSeprtdIds = splitListByComma ids                 
                newBodyExpr = substArg (Body reducedIdsList exprs) (head args) (ids)


substArg :: Expression -> Expression -> [String] -> [Expression]
substArg (Body reducedIds exprs) (argExpr) unmodifiedIds 
                                            | null exprs = exprs ++ [argExpr]
                                            | otherwise = [newIdsExpr] ++ exprsWithoutIdExpr ++ [argExpr]
                                                where
                                                    newIdsExpr = reduceIdsExpr unmodifiedIds oldIdsExpr 
                                                    oldIdsExpr = head exprs 
                                                    exprsWithoutIdExpr = tail exprs
                                                        
reduceIdsExpr :: [String] -> Expression -> Expression
reduceIdsExpr unmodifiedIds idsExpr =
                                let
                                    reducedIdsResult = filter (\x -> (head (splitListByComma unmodifiedIds) == x) == False) bodyIdsList 
                                    bodyIdsList = splitListByComma(fromJust(getExprVars idsExpr)) 
                                in
                                    Var reducedIdsResult

paramMatch :: Expression -> Bool
paramMatch (Body ids exprs) 
            | elem firstId separatedBodyExprVars = True    
            | otherwise = False
            where 
                firstId = head (splitListByComma ids) 
                separatedBodyExprVars = splitListByComma bodyExprVars
                bodyExprVars = getVarList argVarsExpr
                argVarsExpr = head exprs 
