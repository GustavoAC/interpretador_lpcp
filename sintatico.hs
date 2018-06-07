-- Comente para executar o main local
--module Sintatico (TokenTree(..), NonTToken(..), parser) where

import Lexico
import Text.Parsec
import Control.Monad.IO.Class

import System.IO.Unsafe

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
  NonTIf |
  NonTExpr |
  NonTInvokeFunction |
  NonTId |
  NonTInvokeFunctionArgs |
  NonPtrOp |
  NonTArray |
  NonTParam |
  NonTListIndex
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
symBoolNotToken :: ParsecT [Token] st IO (Token)
symBoolNotToken = tokenPrim show update_pos get_token where
  get_token (SymBoolNot pos) = Just (SymBoolNot pos)
  get_token _                = Nothing

symBoolAndToken :: ParsecT [Token] st IO (Token)
symBoolAndToken = tokenPrim show update_pos get_token where
  get_token (SymBoolAnd pos) = Just (SymBoolAnd pos)
  get_token _                = Nothing

symBoolOrToken :: ParsecT [Token] st IO (Token)
symBoolOrToken = tokenPrim show update_pos get_token where
  get_token (SymBoolOr pos) = Just (SymBoolOr pos)
  get_token _               = Nothing

symBoolTrueToken :: ParsecT [Token] st IO (Token)
symBoolTrueToken = tokenPrim show update_pos get_token where
  get_token (SymBoolTrue pos) = Just (SymBoolTrue pos)
  get_token _                = Nothing

symBoolFalseToken :: ParsecT [Token] st IO (Token)
symBoolFalseToken = tokenPrim show update_pos get_token where
  get_token (SymBoolFalse pos) = Just (SymBoolFalse pos)
  get_token _                = Nothing

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

returnToken :: ParsecT [Token] st IO (Token)
returnToken = tokenPrim show update_pos get_token where
  get_token (Return pos) = Just (Return pos)
  get_token _              = Nothing

breakToken :: ParsecT [Token] st IO (Token)
breakToken = tokenPrim show update_pos get_token where
  get_token (Break pos) = Just (Break pos)
  get_token _              = Nothing

continueToken :: ParsecT [Token] st IO (Token)
continueToken = tokenPrim show update_pos get_token where
  get_token (Continue pos) = Just (Continue pos)
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
  get_token (Print pos)  = Just (Print pos)
  get_token _            = Nothing

procedureToken :: ParsecT [Token] st IO (Token)
procedureToken = tokenPrim show update_pos get_token where
  get_token (Procedure pos) = Just (Procedure pos)
  get_token _               = Nothing

functionToken :: ParsecT [Token] st IO (Token)
functionToken = tokenPrim show update_pos get_token where
  get_token (Function pos) = Just (Function pos)
  get_token _              = Nothing


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
stmts = try (
  do
    a <- stmt
    b <- stmts
    return (DualTree NonTStatements a b)
  ) <|> (
  do
    a <- stmt
    return (UniTree NonTStatement a)
  )

stmt :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
stmt = do
       first <- assign
       colon <- semicolonToken
       return (UniTree NonTStatement first)

assign :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
assign = do
          a <- idToken
          b <- attribToken
          c <- expr0
          return (TriTree NonTAssign (makeToken a) (makeToken b) c)

-- &&  ||
expr0 :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
expr0 = try (
  do
    a <- openParenthToken
    meioParent <- expr0
    b <- closeParenthToken
    meio <- expr0Ops
    c <- expr0
    return (TriTree NonTExpr meioParent meio c)
  ) <|> try (
  do
    a <- expr1
    meio <- expr0Ops
    b <- expr0
    return (TriTree NonTExpr a meio b)
  ) <|> try (
  do
    a <- expr1
    return a
  )

expr0Ops :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
expr0Ops = 
  (do 
    sym <- symBoolAndToken
    return (makeToken sym)
  ) <|> (do
    sym <- symBoolOrToken
    return (makeToken sym)
  )

-- !
expr1 :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
expr1 = try (
  do
    meio <- expr1Ops
    c <- expr1
    return (DualTree NonTExpr meio c)
  ) <|> try (
  do
    a <- expr2
    return a
  )

expr1Ops :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
expr1Ops = 
  (do
    sym <- symBoolNotToken
    return (makeToken sym)
  )

-- <  >  <=  >=  ==  !=
expr2 :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
expr2 = try (
  do
    a <- openParenthToken
    meioParent <- expr0
    b <- closeParenthToken
    meio <- expr2Ops
    c <- expr2
    return (TriTree NonTExpr meioParent meio c)
  ) <|> try (
  do
    a <- expr3
    meio <- expr2Ops
    b <- expr2
    return (TriTree NonTExpr a meio b)
  ) <|> (
  do
    a <- expr3
    return a
  )

expr2Ops :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
expr2Ops = 
  (do
    sym <- symBoolLessThanToken
    return (makeToken sym)
  ) <|> (do
    sym <- symBoolGreaterThanToken
    return (makeToken sym)
  ) <|> (do
    sym <- symBoolLessThanEqToken
    return (makeToken sym)
  ) <|> (do
    sym <- symBoolGreaterThanEqToken
    return (makeToken sym)
  ) <|> (do
    sym <- symBoolEqToken
    return (makeToken sym)
  ) <|> (do
    sym <- symBoolNotEqToken
    return (makeToken sym)
  )

-- +  -
expr3 :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
expr3 = try (
  do
    a <- openParenthToken
    meioParent <- expr0
    b <- closeParenthToken
    meio <- expr3Ops
    c <- expr3
    return (TriTree NonTExpr meioParent meio c)
  ) <|> try (
  do
    a <- expr4
    meio <- expr3Ops
    b <- expr3
    return (TriTree NonTExpr a meio b)
  ) <|> (
  do
    a <- expr4
    return a
  )

expr3Ops :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
expr3Ops = (do
    sym <- symOpPlusToken
    return (makeToken sym)
  ) <|> (do
    sym <- symOpMinusToken
    return (makeToken sym)
  )

-- *  /  %
expr4 :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
expr4 = try (
  do
    a <- openParenthToken
    meioParent <- expr0
    b <- closeParenthToken
    meio <- expr4Ops
    c <- expr4
    return (TriTree NonTExpr meioParent meio c)
  ) <|> try (
  do
    a <- expr5
    meio <- expr4Ops
    b <- expr4
    return (TriTree NonTExpr a meio b)
  ) <|> (
  do
    a <- expr5
    return a
  )
 
expr4Ops :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
expr4Ops = (do
    sym <- symOpMultToken
    return (makeToken sym)
  ) <|> (do
    sym <- symOpDivToken
    return (makeToken sym)
  ) <|> (do
    sym <- symOpModToken
    return (makeToken sym)
  )


-- ^
expr5 :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
expr5 = try (
  do
    a <- openParenthToken
    meioParent <- expr0
    b <- closeParenthToken
    meio <- expr5Ops
    c <- expr5
    return (TriTree NonTExpr meioParent meio c)
  ) <|> try (
  do
    a <- exprParenth
    meio <- expr5Ops
    b <- expr5
    return (TriTree NonTExpr a meio b)
  ) <|> (
  do
    a <- exprParenth
    return a
  )

expr5Ops :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
expr5Ops = (do
    sym <- symOpExpToken
    return (makeToken sym)
  )

-- ( )
exprParenth :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
exprParenth = try (
  -- ( )
  do
    a <- openParenthToken
    meio <- expr0
    b <- closeParenthToken
    return meio
  ) <|> (
    do
      a <- exprFinalIds
      return a
  ) 

-- function, ids e literis
exprFinalIds :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
exprFinalIds = try (
  -- function
  do
    a <- exprFunction
    return (UniTree NonTInvokeFunction a)
  ) <|> try (
  -- id
  do
    a <- exprId
    return (UniTree NonTId a)
  ) <|> try (
  -- strlit
  do
    a <- strLitToken
    return (LeafToken a)
  ) <|> try (
  -- floatlit
  do
    a <- floatLitToken
    return (LeafToken a)
  ) <|> (
  -- intlit
  do
    a <- intLitToken
    return (LeafToken a)
  )

-- Função
exprFunction :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
exprFunction = try (
  -- a(3, 4, ...)
  do 
    name <- idToken
    a <- openParenthToken
    b <- listParam
    c <- closeParenthToken
    return (DualTree NonTInvokeFunctionArgs (makeToken name) b ) -- ?
  ) <|> (
  -- a()
  do
    name <- idToken
    a <- openParenthToken
    b <- closeParenthToken
    return (LeafToken name) -- ?
  )

listParam :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
listParam = try (
  -- param, ... , param
  do
    a <- expr0
    b <- semicolonToken
    c <- listParam
    return (DualTree NonTParam a c) -- ?
  ) <|> (
  -- param
  do 
    a <- expr0
    return a
  )

exprId :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
exprId = try (
  -- $a 
  do 
    a <- symPtrOpToken
    b <- exprId
    return (UniTree NontPtrOp b) -- ?
  ) <|> try (
  -- a[] 
  do 
    a <- idToken -- Substituir por um nome mesmo, não só um token
    b <- listIndexes
    return (DualTree NonTArray (makeToken a) b) -- ?
  ) <|> (
  -- a
  do 
    a <- idToken -- Substituir por um nome mesmo, não só um token
    return (LeafToken a)
  )

listIndexes :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
listIndexes = try (
  -- [x]...[]
  do
    a <- openBracketToken
    b <- expr0
    c <- closeBracketToken
    d <- listIndexes
    return ( DualTree NonTListIndex (makeToken b) d) -- ?
  ) <|> (
  -- [x]
  do
    a <- openBracketToken
    b <- expr0
    c <- closeBracketToken
    return b
  )
  
-- Main e função que chama o parser

parser :: [Token] -> IO (Either ParseError TokenTree)
parser tokens = runParserT program [] "Error message" tokens

-- Descomente para usar o main local
main :: IO ()
main = case unsafePerformIO (parser (getTokens "arquivo.in")) of
            { Left err -> print err; 
              Right ans -> print ans
            }