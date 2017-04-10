import qualified HappyParser
import Expressions
import Interpreter

main :: IO ()
main = do
  input <- getContents
  let parsedInput = HappyParser.parseExpression input
  print $ show parsedInput
  print $ interpret parsedInput