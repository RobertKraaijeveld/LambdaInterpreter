import qualified HappyParser
import Expressions
import Interpreter

main :: IO ()
main = do
  input <- getContents
  let parsedInput = HappyParser.parseExpression input
  let args = getArgs parsedInput
  print $ show args
  print $ interpret parsedInput args