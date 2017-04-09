import qualified HappyParser
import Expressions
import Interpreter

{-

data Expression = Body Id Expression
    | Application Expression Expression
    | Var Id
    | Num Int
    | Binop Operator Expression Expression
    deriving (Eq, Show)
    --find out how deriving eq works


| DENOTES AN ENUMERATION OF POSSIBLE VALUES!

    boolToInt :: Bool -> Int
boolToInt b = if b then 1 else 0

main = print $ boolToInt False
-}


main :: IO ()
main = do
  input <- getContents
  let x = HappyParser.parseExpression input
  if (getBody x) == Nothing then print "nothing" else print $ toString (fromJust (getBody x))
  print $ splitLambdaParameters (==',') "x,y"