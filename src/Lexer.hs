module Lexer where 

import Data.Char (isDigit, isLetter, isSpace, isLower, isUpper, digitToInt)

data Token
  = PlusTok | MinusTok | TimesTok
  | OpenTok | CloseTok | SemiColonTok
  | AndTok | BoolEqTok | IntEqTok | LeTok | NotTok
  | AssignTok
  | WhileTok | DoTok
  | IfTok | ThenTok | ElseTok
  | IntTok Integer | VarTok String | BoolTok Bool
  | ForTok
  deriving (Show, Eq) 

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