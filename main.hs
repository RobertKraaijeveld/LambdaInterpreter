import qualified HappyParser
import Expressions
import Interpreter

main :: IO ()
main = do
  input <- getContents
  let parsedInput = HappyParser.parseExpression input
  print $ interpret parsedInput