{
module HappyParser where

import Data.Char
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
    '/' { TokenLambda }    
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
    | '/' VAR '->' Expression { Body $2 $4 }
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


    -- LEXER
      
    -- LEXER
    lexer :: String -> [Token]
    lexer [] = []
    lexer (c:cs)
    | isSpace c = lexer cs
    | isAlpha c = lexVar (c:cs)
    | isDigit c = lexNum (c:cs)

    lexer ('/':cs) = TokenLambda : lexer cs
    lexer ('=':cs) = TokenArrow : lexer cs
    lexer ('=':cs) = TokenEq : lexer cs
    lexer ('+':cs) = TokenPlus : lexer cs
    lexer ('-':cs) = TokenMinus : lexer cs
    lexer ('*':cs) = TokenTimes : lexer cs
    lexer ('(':cs) = TokenOB : lexer cs
    lexer (')':cs) = TokenCB : lexer cs

    lexNum cs = TokenNum (read num) : lexer rest
        where (num,rest) = span isDigit cs

    lexVar cs =
        case span isAlpha cs of
            ("let",rest) -> TokenLet : lexer rest
            ("in",rest) -> TokenIn : lexer rest
            (var,rest) -> TokenVar var : lexer rest



    -- 'main' function
    main getContents >>- print . parse . lexer
}
