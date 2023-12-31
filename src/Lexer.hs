{-|
  Module      : Lexer
  Description : Acts as the project's lexer by tokenizing inputs strings with a program
  Copyright   : Daniel Ferreira and Mansur Mustafin, 2023
  Maintainer  : unidsferreira2003@gmail.com and mustafin.mansur02@gmail.com

  This module holds functions necessary to extract the tokens from input strings that hold
  valid programs. It also rejects any program that contains invalid tokens
-}

module Lexer where 

import Data.Char (isDigit, isLetter, isSpace, isLower, isUpper, digitToInt)

{-|
    Holds all valid tokens that can be used in a program. Each token has the name of the corresponding operator or constant
    followed by the suffix "Tok"
-}
data Token
  = PlusTok | MinusTok | TimesTok
  | OpenTok | CloseTok | SemiColonTok
  | AndTok | BoolEqTok | IntEqTok | LeTok | NotTok
  | AssignTok
  | WhileTok | DoTok
  | IfTok | ThenTok | ElseTok
  | IntTok Integer | VarTok String | BoolTok Bool
  | ForTok
  | OpenSqTok | CloseSqTok | CommaTok  
  deriving (Show, Eq) 

{-|
    Translates a program in a string to a list of corresponding valid tokens. 
    
    If an invalid token is found an error is displayed.
-}
lexer :: String -> [Token]
lexer [] = []
lexer ('+':restStr) = PlusTok : lexer restStr
lexer ('-':restStr) = MinusTok : lexer restStr
lexer ('*':restStr) = TimesTok : lexer restStr
lexer ('(':restStr) = OpenTok : lexer restStr
lexer (')':restStr) = CloseTok : lexer restStr
lexer (';':restStr) = SemiColonTok : lexer restStr
lexer ('=':'=':restStr) = IntEqTok : lexer restStr
lexer ('=':restStr) = BoolEqTok : lexer restStr
lexer ('<':'=':restStr) = LeTok : lexer restStr
lexer (':':'=':restStr) = AssignTok : lexer restStr

lexer ('[':restStr) = OpenSqTok : lexer restStr
lexer (']':restStr) = CloseSqTok : lexer restStr
lexer (',':restStr) = CommaTok : lexer restStr

lexer str@(char:restStr)
 | isSpace char = lexer restStr
 | isDigit char = let (digStr, restStr) = span isDigit str
                      stringToInt = foldl (\acc chr -> 10 * acc + fromIntegral (digitToInt chr)) 0
                  in if null restStr || not (head restStr == '_' || isLetter (head restStr))
                      then IntTok (stringToInt digStr) : lexer restStr
                     else error "Syntax error: Variables cannot start with a digit"
 | isLetter char = let (varStr, restStr) = span (\x -> isLetter x || isDigit x || x == '_') str
                   in getWordToken varStr : lexer restStr
 | otherwise = error "Syntax error: Invalid symbol"

{-|
    Transforms a word into its corresponding token. The word may be one of the reserved keywords
    ("not", "and", etc.) or a valid name for a variable, that is, a word that starts with an undercased letter
    followed by any number of letters, digits and underscores (_).
-}
getWordToken :: String -> Token
getWordToken "and" = AndTok
getWordToken "not" = NotTok
getWordToken "while" = WhileTok
getWordToken "do" = DoTok
getWordToken "if" = IfTok
getWordToken "then" = ThenTok
getWordToken "else" = ElseTok
getWordToken "for" = ForTok
getWordToken "True" = BoolTok True
getWordToken "False" = BoolTok False
getWordToken str@(first:rest) | isLower first = VarTok str
getWordToken _ = error "Syntax error: Invalid symbol"