import qualified HappyParser
import Expressions
import Interpreter

main :: IO ()
main = do
  input <- getContents
  let parsedInput = HappyParser.parseExpression input
  let testNum = (Num 6)
  let testMulti = (MultiExpression [testNum])
  let testBody = (Body ["x"] testMulti)
  let testArg = (Num 7)
  let test = reduceBody testBody testArg
  print $ test
  print $ interpret parsedInput