import qualified HappyParser
import Expressions
import Interpreter

collectAllBodies :: Expression -> [Expression] -> [Expression]
collectAllBodies rootExpr args
                        | anyBodies rootExpr == True = [fromJust(maybeInitialBody)] ++ getBodiesFromArgs args
                        | otherwise = []
                        where 
                          maybeInitialBody = getBody rootExpr

main :: IO ()
main = do
  input <- getContents
  let parsedInput = HappyParser.parseExpression input
  let args = getArgs parsedInput
  let bodies = collectAllBodies parsedInput args   

  putStrLn "ARGS: "
  putStrLn $ show args  ++ "\n"  

  putStrLn "BODIES: "
  putStrLn $ show bodies  ++ "\n"  --needs to be done for all in list

--getBodiesFromArgs args
  putStrLn "ORIGINAL: "
  putStrLn $ show parsedInput  ++ "\n"

  putStrLn "RESULT: "  
  putStrLn $ show (interpret (reverse bodies) args)