module Sintatico (TokenTree(..), NonTToken(..), parser) where

import Lexico
import Text.Parsec
import Control.Monad.IO.Class

data TokenTree = QuadTree NonTToken TokenTree TokenTree TokenTree TokenTree |
                 TriTree NonTToken TokenTree TokenTree TokenTree |
                 DualTree NonTToken TokenTree TokenTree |
                 UniTree NonTToken TokenTree |
                 None |
                 LeafToken Token deriving (Eq, Show)
data NonTToken = 
  NonTProgram |
  NonTStatements |
  NonTStatement |
  NonTAssign |
  NonTIf
  deriving (Eq, Show)

makeToken :: Token -> TokenTree
makeToken tok = LeafToken tok

-- parsers para os tokens

--
-- Tipos
--
typeIntToken :: ParsecT [Token] st IO (Token)
typeIntToken = tokenPrim show update_pos get_token where
  get_token (TypeInt pos) = Just (TypeInt pos)
  get_token _             = Nothing

typeFloatToken :: ParsecT [Token] st IO (Token)
typeFloatToken = tokenPrim show update_pos get_token where
  get_token (TypeFloat pos)   = Just (TypeFloat pos)
  get_token _                 = Nothing

typeStringToken :: ParsecT [Token] st IO (Token)
typeStringToken = tokenPrim show update_pos get_token where
  get_token (TypeString pos)    = Just (TypeString pos)
  get_token _                   = Nothing

typePointerToken :: ParsecT [Token] st IO (Token)
typePointerToken = tokenPrim show update_pos get_token where
  get_token (TypePointer pos)   = Just (TypePointer pos)
  get_token _                   = Nothing

typeBooleanToken :: ParsecT [Token] st IO (Token)
typeBooleanToken = tokenPrim show update_pos get_token where
  get_token (TypeBoolean pos) = Just (TypeBoolean pos)
  get_token _                 = Nothing

-- 
-- Literais
-- 

idToken :: ParsecT [Token] st IO (Token)
idToken = tokenPrim show update_pos get_token where
  get_token (Id pos x) = Just (Id pos x)
  get_token _           = Nothing

intLitToken :: ParsecT [Token] st IO (Token)
intLitToken = tokenPrim show update_pos get_token where
  get_token (IntLit pos x)      = Just (IntLit pos x)
  get_token _                 = Nothing
  
floatLitToken :: ParsecT [Token] st IO (Token)
floatLitToken = tokenPrim show update_pos get_token where
  get_token (FloatLit pos x)    = Just (FloatLit pos x)
  get_token _                 = Nothing

strLitToken :: ParsecT [Token] st IO (Token)
strLitToken = tokenPrim show update_pos get_token where
  get_token (StrLit pos x) = Just (StrLit pos x)
  get_token _              = Nothing

-- 
-- Símbolos Booleanos
-- 
symBoolEqToken :: ParsecT [Token] st IO (Token)
symBoolEqToken = tokenPrim show update_pos get_token where
  get_token (SymBoolEq pos)   = Just (SymBoolEq pos)
  get_token _                 = Nothing

symBoolNotEqToken :: ParsecT [Token] st IO (Token)
symBoolNotEqToken = tokenPrim show update_pos get_token where
  get_token (SymBoolNotEq pos) = Just (SymBoolNotEq pos)
  get_token _                  = Nothing

symBoolLessThanEqToken :: ParsecT [Token] st IO (Token)
symBoolLessThanEqToken = tokenPrim show update_pos get_token where
  get_token (SymBoolLessThanEq pos) = Just (SymBoolLessThanEq pos)
  get_token _                       = Nothing

symBoolGreaterThanEqToken :: ParsecT [Token] st IO (Token)
symBoolGreaterThanEqToken = tokenPrim show update_pos get_token where
  get_token (SymBoolGreaterThanEq pos) = Just (SymBoolGreaterThanEq pos)
  get_token _                          = Nothing

symBoolAndToken :: ParsecT [Token] st IO (Token)
symBoolAndToken = tokenPrim show update_pos get_token where
  get_token (SymBoolAnd pos) = Just (SymBoolAnd pos)
  get_token _                = Nothing

symBoolOrToken :: ParsecT [Token] st IO (Token)
symBoolOrToken = tokenPrim show update_pos get_token where
  get_token (SymBoolOr pos) = Just (SymBoolOr pos)
  get_token _               = Nothing

symBoolLessThanToken :: ParsecT [Token] st IO (Token)
symBoolLessThanToken = tokenPrim show update_pos get_token where
  get_token (SymBoolLessThan pos) = Just (SymBoolLessThan pos)
  get_token _                     = Nothing

symBoolGreaterThanToken :: ParsecT [Token] st IO (Token)
symBoolGreaterThanToken = tokenPrim show update_pos get_token where
  get_token (SymBoolGreaterThan pos) = Just (SymBoolGreaterThan pos)
  get_token _                        = Nothing

--
-- Símbolos de operações
--
symOpPlusToken :: ParsecT [Token] st IO (Token)
symOpPlusToken = tokenPrim show update_pos get_token where
  get_token (SymOpPlus pos) = Just (SymOpPlus pos)
  get_token _               = Nothing

symOpMinusToken :: ParsecT [Token] st IO (Token)
symOpMinusToken = tokenPrim show update_pos get_token where
  get_token (SymOpMinus pos) = Just (SymOpMinus pos)
  get_token _                = Nothing

symOpMultToken :: ParsecT [Token] st IO (Token)
symOpMultToken = tokenPrim show update_pos get_token where
  get_token (SymOpMult pos) = Just (SymOpMult pos)
  get_token _               = Nothing

symOpDivToken :: ParsecT [Token] st IO (Token)
symOpDivToken = tokenPrim show update_pos get_token where
  get_token (SymOpDiv pos)  = Just (SymOpDiv pos)
  get_token _               = Nothing

symOpExpToken :: ParsecT [Token] st IO (Token)
symOpExpToken = tokenPrim show update_pos get_token where
  get_token (SymOpExp pos)  = Just (SymOpExp pos)
  get_token _               = Nothing

symOpModToken :: ParsecT [Token] st IO (Token)
symOpModToken = tokenPrim show update_pos get_token where
  get_token (SymOpMod pos)  = Just (SymOpMod pos)
  get_token _               = Nothing

--
-- Brackets e afins
--
openParenthToken :: ParsecT [Token] st IO (Token)
openParenthToken = tokenPrim show update_pos get_token where
  get_token (OpenParenth pos) = Just (OpenParenth pos)
  get_token _                 = Nothing

closeParenthToken :: ParsecT [Token] st IO (Token)
closeParenthToken = tokenPrim show update_pos get_token where
  get_token (CloseParenth pos) = Just (CloseParenth pos)
  get_token _                  = Nothing

openBracketToken :: ParsecT [Token] st IO (Token)
openBracketToken = tokenPrim show update_pos get_token where
  get_token (OpenBracket pos) = Just (OpenBracket pos)
  get_token _                 = Nothing

closeBracketToken :: ParsecT [Token] st IO (Token)
closeBracketToken = tokenPrim show update_pos get_token where
  get_token (CloseBracket pos) = Just (CloseBracket pos)
  get_token _                  = Nothing

openScopeToken :: ParsecT [Token] st IO (Token)
openScopeToken = tokenPrim show update_pos get_token where
  get_token (OpenScope pos) = Just (OpenScope pos)
  get_token _               = Nothing

closeScopeToken :: ParsecT [Token] st IO (Token)
closeScopeToken = tokenPrim show update_pos get_token where
  get_token (CloseScope pos) = Just (CloseScope pos)
  get_token _                = Nothing

--
-- Ponteiro e endereçamento
--
symPtrOpToken :: ParsecT [Token] st IO (Token)
symPtrOpToken = tokenPrim show update_pos get_token where
  get_token (SymPtrOp pos) = Just (SymPtrOp pos)
  get_token _                = Nothing

symAdressOpToken :: ParsecT [Token] st IO (Token)
symAdressOpToken = tokenPrim show update_pos get_token where
  get_token (SymAdressOp pos) = Just (SymAdressOp pos)
  get_token _                = Nothing

--
-- Estruturas de controle
--
forToken :: ParsecT [Token] st IO (Token)
forToken = tokenPrim show update_pos get_token where
  get_token (For pos) = Just (For pos)
  get_token _         = Nothing

endForToken :: ParsecT [Token] st IO (Token)
endForToken = tokenPrim show update_pos get_token where
  get_token (EndFor pos) = Just (EndFor pos)
  get_token _            = Nothing

whileToken :: ParsecT [Token] st IO (Token)
whileToken = tokenPrim show update_pos get_token where
  get_token (While pos) = Just (While pos)
  get_token _           = Nothing

endWhileToken :: ParsecT [Token] st IO (Token)
endWhileToken = tokenPrim show update_pos get_token where
  get_token (EndWhile pos) = Just (EndWhile pos)
  get_token _              = Nothing

ifToken :: ParsecT [Token] st IO (Token)
ifToken = tokenPrim show update_pos get_token where
  get_token (If pos) = Just (If pos)
  get_token _           = Nothing

endIfToken :: ParsecT [Token] st IO (Token)
endIfToken = tokenPrim show update_pos get_token where
  get_token (EndIf pos) = Just (EndIf pos)
  get_token _              = Nothing

--
-- Statements
--
semicolonToken :: ParsecT [Token] st IO (Token)
semicolonToken = tokenPrim show update_pos get_token where
  get_token (Semicolon pos) = Just (Semicolon pos)
  get_token _               = Nothing

attribToken :: ParsecT [Token] st IO (Token)
attribToken = tokenPrim show update_pos get_token where
  get_token (Attrib pos) = Just (Attrib pos)
  get_token _            = Nothing

printToken :: ParsecT [Token] st IO (Token)
printToken = tokenPrim show update_pos get_token where
  get_token (Print pos) = Just (Print pos)
  get_token _            = Nothing

-- O que ele quis dizer com isso?
update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos 

-- Parsers não terminais

--         Parsec   input       state         output
program :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
program = do
            -- a <- funcDecs
            b <- stmts
            eof
            return (UniTree NonTProgram b)

stmts :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
stmts = (do a <- stmt
            b <- stmts
            return (DualTree NonTStatements a b)) <|> (return None)

stmt :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
stmt = do
       first <- assign
       colon <- semicolonToken
       return (UniTree NonTStatement first)

assign :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
assign = do
          a <- idToken
          b <- attribToken
          c <- strLitToken
          return (TriTree NonTAssign (makeToken a) (makeToken b) (makeToken c))

-- Main e função que chama o parser

parser :: [Token] -> IO (Either ParseError TokenTree)
parser tokens = runParserT program [] "Error message" tokens