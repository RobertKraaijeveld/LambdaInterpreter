{
{-# OPTIONS_GHC -w #-}
module AlexLexer (Token(..),scanTokens) where
import Expressions
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-
  \,                             ;
  $eol                          ;
  $white+                       ;
  "#".*                         ;
  let                           { \s -> TokenLet }
  in                            { \s -> TokenIn }
  $digit+                       { \s -> TokenNum (read s) }
  "->"                          { \s -> TokenArrow }
  \=                            { \s -> TokenEq }
  \\                            { \s -> TokenLambda }
  [\+]                          { \s -> TokenPlus }
  [\-]                          { \s -> TokenMinus }
  [\*]                          { \s -> TokenTimes }
  \(                            { \s -> TokenOB }
  \)                            { \s -> TokenCB }
  $alpha [$alpha $digit \_ \']* { \s -> TokenVar s }


{
data Token = TokenLet
    | TokenIn
    | TokenNum Int
    | TokenVar String
    | TokenEq 
    | TokenLambda
    | TokenArrow     
    | TokenPlus
    | TokenMinus
    | TokenTimes
    | TokenOB 
    | TokenCB 
    deriving (Eq,Show)

scanTokens = alexScanTokens
}