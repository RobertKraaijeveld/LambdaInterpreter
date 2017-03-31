{
module HappyParser where

import Expressions
import Lexer
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    let { TokenLet }
    in { TokenIn }
    NUM { TokenNum $$ }    
    VAR { TokenVar $$ }
    '/' { TokenLambda }    
    '->' { TokenArrow }  
    '='   { TokenEq }  
    '+' { TokenPlus }
    '-' { TokenMinus }
    '*' { TokenTimes }
    '(' { TokenOB }
    ')' { TokenCB }

%%

Expression: let VAR '=' Expression in Expression { Application (Body $2 $6) $4 }
    | '/' VAR '->' Expression { Body $2 $4 }
    | Form

Form : Form '+' Form { Binop Add $1 $3 }
    | Form '-' Form { Binop Sub $1 $3 }
    | Form '*' Form { Binop Mul $1 $3 }
    | Tuple { $1 }

Tuple: Tuple Single { Application $1 $2 }
    | Single { $1 }
 
 
{
    parseError :: [Token] -> a
    parseError _ = error "parsing error" 

    main getContents >>- print . parse . lexer
}
