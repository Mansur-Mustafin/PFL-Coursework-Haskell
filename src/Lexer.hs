module Lexer where 

import Data.Char (isDigit, isLetter, isSpace, isLower, isUpper, digitToInt)

data Token
  = PlusTok | MinusTok | TimesTok
  | OpenTok | CloseTok | SemiColonTok
  | AndTok | BoolEqTok | IntEqTok | LeTok | NotTok
  | AssignTok
  | WhileTok | DoTok
  | IfTok | ThenTok | ElseTok
  | IntTok Int | VarTok String | BoolTok Bool
  deriving (Show) 

lexer :: String -> [Token]
lexer [] = []
lexer ('+':restStr) = PlusTok : lexer restStr
lexer ('-':restStr) = MinusTok : lexer restStr
lexer ('*':restStr) = TimesTok : lexer restStr
lexer ('(':restStr) = OpenTok : lexer restStr
lexer (')':restStr) = CloseTok : lexer restStr
lexer (';':restStr) = SemiColonTok : lexer restStr

lexer ('a':'n':'d':' ':restStr) = AndTok : lexer restStr
lexer ('a':'n':'d':'(':restStr) = AndTok : OpenTok : lexer restStr

lexer ('=':'=':restStr) = IntEqTok : lexer restStr
lexer ('=':restStr) = BoolEqTok : lexer restStr
lexer ('<':'=':restStr) = LeTok : lexer restStr

lexer ('n':'o':'t':' ':restStr) = NotTok : lexer restStr
lexer ('n':'o':'t':'(':restStr) = NotTok : OpenTok : lexer restStr

lexer (':':'=':restStr) = AssignTok : lexer restStr

lexer ('w':'h':'i':'l':'e':' ':restStr) = WhileTok : lexer restStr
lexer ('w':'h':'i':'l':'e':'(':restStr) = WhileTok : OpenTok : lexer restStr

lexer ('d':'o':' ':restStr) = DoTok : lexer restStr
lexer ('d':'o':'(':restStr) = DoTok : OpenTok : lexer restStr

lexer ('i':'f':' ':restStr) = IfTok : lexer restStr
lexer ('i':'f':'(':restStr) = IfTok : OpenTok : lexer restStr

lexer ('t':'h':'e':'n':' ':restStr) = ThenTok : lexer restStr
lexer ('t':'h':'e':'n':'(':restStr) = ThenTok : OpenTok : lexer restStr

lexer ('e':'l':'s':'e':' ':restStr) = ElseTok : lexer restStr
lexer ('e':'l':'s':'e':'(':restStr) = ElseTok : OpenTok : lexer restStr

lexer ('T':'r':'u':'e':' ':restStr) = BoolTok True : lexer restStr
lexer ('T':'r':'u':'e':';':restStr) = BoolTok True : SemiColonTok : lexer restStr
lexer ('T':'r':'u':'e':')':restStr) = BoolTok True : CloseTok : lexer restStr

lexer ('F':'a':'l':'s':'e':' ':restStr) = BoolTok False : lexer restStr
lexer ('F':'a':'l':'s':'e':';':restStr) = BoolTok False : SemiColonTok : lexer restStr
lexer ('F':'a':'l':'s':'e':')':restStr) = BoolTok False : CloseTok : lexer restStr


lexer str@(char:restStr)
 | isSpace char = lexer restStr
 | isDigit char = let (digStr, restStr) = span isDigit str
                      stringToInt = foldl (\acc chr->10*acc+digitToInt chr) 0
                  in if null restStr || not (head restStr == '_' || isLetter (head restStr))
                      then IntTok (stringToInt digStr) : lexer restStr
                     else error "Syntax error: Variables cannot start with a digit"
 | isLower char = let (varStr, restStr) = span (\x -> isLetter x || isDigit x || x == '_') str
                   in VarTok varStr : lexer restStr
 | otherwise = error "Syntax error: Invalid symbol"

