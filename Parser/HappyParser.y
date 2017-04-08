{
module HappyParser where

import Data.Char
import AlexLexer
import Expressions
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    let { TokenLet }
    in { TokenIn }
    NUM { TokenNum $$ }    
    VAR { TokenVar $$ }
    '\\' { TokenLambda }    
    '->' { TokenArrow }  
    '='   { TokenEq }  
    '+' { TokenPlus }
    '-' { TokenMinus }
    '*' { TokenTimes }
    '(' { TokenOB }
    ')' { TokenCB }

%left '+' '-'
%left '*'
%%

Expression: let VAR '=' Expression in Expression { Application (Body $2 $6) $4 }
    | '\\' VAR '->' Expression { Body $2 $4 }
    | Form { $1 }

Form : Form '+' Form { Binop Add $1 $3 }
    | Form '-' Form { Binop Sub $1 $3 }
    | Form '*' Form { Binop Mul $1 $3 }
    | Tuple { $1 }

Tuple: Tuple Single { Application $1 $2 }
    | Single { $1 }

Single: '(' Expression ')' { $2 }
    | NUM { Num $1 }
    | VAR { Var $1 }    
 
{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseExpression :: String -> Expression
parseExpression = parse . scanTokens
}
