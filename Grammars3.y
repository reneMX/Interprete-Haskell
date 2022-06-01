{
module Grammars3 where

import Data.Char
}

%name parse
%tokentype { Token }
%error { parseError }

%token
      var             { TokenId $$ }
      int             { TokenNum $$ }
      '+'             { TokenSuma }
      '-'             { TokenResta }
      '*'             { TokenMult }
      '/'             { TokenDiv }
      '('             { TokenPA }
      ')'             { TokenPC }
      let             { TokenLet }
      lambda          { TokenLambda }

%%

ASA : var                  { Id $1 }
    | int                  { Num $1 }
    | '(' '+' ASA ASA ')'  { Suma $3 $4}
    | '(' '-' ASA ASA ')'  { Resta $3 $4}
    | '(' '*' ASA ASA ')'  { Mult $3 $4}
    | '(' '/' ASA ASA ')'  { Div $3 $4}
    | '(' let '(' var ASA ')' ASA ')' { Let $4 $5 $7 }
    | '(' lambda '(' var ASA ')' ')' { Lambda $4 $5 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"


data ASA = Id String
          | Num Int
          | Suma ASA ASA
          | Resta ASA ASA
          | Mult ASA ASA
          | Div ASA ASA
          | Let String ASA ASA
          | Lambda String ASA
          | App ASA ASA
          deriving(Show, Eq) 

data Token = TokenId String
           | TokenNum Int
           | TokenSuma
           | TokenResta
           | TokenMult
           | TokenDiv
           | TokenPA
           | TokenPC
           | TokenLet
           | TokenLambda
           deriving(Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer (' ' : xs) = lexer xs
lexer ('(' : xs) = TokenPA:(lexer xs)
lexer (')' : xs) = TokenPC:(lexer xs)
lexer ('+' : xs) = TokenSuma:(lexer xs)
lexer ('-' : xs) = TokenResta:(lexer xs)
lexer ('*' : xs) = TokenMult:(lexer xs)
lexer ('/' : xs) = TokenDiv:(lexer xs)
lexer ('l':'e':'t' : xs) = TokenLet:(lexer xs)
lexer ('l':'a':'m':'b':'d':'a' : xs) = TokenLambda:(lexer xs)
lexer (x:xs)
    | isDigit x = lexNum (x:xs)
    | isAlpha x = lexAlpha (x:xs)

lexNum :: String -> [Token]
lexNum cs = TokenNum (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexAlpha :: String -> [Token]
lexAlpha cs = TokenId var : lexer rest
      where (var, rest) = span isAlpha cs

main = getContents >>= print . parse . lexer

}
