import qualified HappyParser
import Expressions

--main getContents >>- print . parse . lexer

main :: IO ()
main = do
  input <- getContents
  print (HappyParser.parseExpression input)