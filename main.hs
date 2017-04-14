import qualified HappyParser
import Expressions
import Interpreter

main :: IO ()
main = do
  input <- getContents
  let parsedInput = HappyParser.parseExpression input
  let args = getArgs parsedInput
  putStrLn "ORIGINAL: "
  putStrLn $ toString parsedInput  ++ "\n"

  putStrLn "RESULT: "  
  putStrLn $ interpret parsedInput args