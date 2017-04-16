import qualified HappyParser
import Expressions
import Interpreter

import Debug.Trace
import Data.List


{-
  MAIN
-}

main :: IO ()
main = do
  input <- getContents
  let parsedInput = HappyParser.parseExpression input
  let args = getArgs parsedInput
  let bodies = collectAllBodies parsedInput args   

  let result = interpret (reverse bodies) args
  let combinedBodies = combineBodies result

  putStrLn "INPUT: "
  putStrLn $ toString parsedInput  ++ "\n"

  putStrLn "RESULT: "
  putStrLn $ combinedBodies ++ "\n"
 