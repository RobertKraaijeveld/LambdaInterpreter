import qualified HappyParser
import Expressions
import Interpreter

main :: IO ()
main = do
  input <- getContents
  let x = HappyParser.parseExpression input
  print x
  print $ splitLambdaParameters (==',') "x,y"