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
  NonTIf |
  NonTExpr
  deriving (Eq, Show)

makeToken :: Token -> TokenTree
makeToken tok = LeafToken tok

-- parsers para os tokens
typeIntToken :: ParsecT [Token] st IO (Token)
typeIntToken = tokenPrim show update_pos get_token where
  get_token (TypeInt pos) = Just (TypeInt pos)
  get_token _             = Nothing

typeBooleanToken :: ParsecT [Token] st IO (Token)
typeBooleanToken = tokenPrim show update_pos get_token where
  get_token (TypeBoolean pos) = Just (TypeBoolean pos)
  get_token _                 = Nothing

-- typeRealToken :: ParsecT [Token] st IO (Token)
-- typeRealToken = tokenPrim show update_pos get_token where
--   get_token (TypeReal pos)    = Just (TypeReal pos)
--   get_token _                 = Nothing

typeStringToken :: ParsecT [Token] st IO (Token)
typeStringToken = tokenPrim show update_pos get_token where
  get_token (TypeString pos)    = Just (TypeString pos)
  get_token _                   = Nothing

semicolonToken = tokenPrim show update_pos get_token where
  get_token (Semicolon pos) = Just (Semicolon pos)
  get_token _               = Nothing

attribToken = tokenPrim show update_pos get_token where
  get_token (Attrib pos) = Just (Attrib pos)
  get_token _            = Nothing

idToken = tokenPrim show update_pos get_token where
  get_token (Id pos x) = Just (Id pos x)
  get_token _           = Nothing

strLitToken = tokenPrim show update_pos get_token where
  get_token (StrLit pos x) = Just (StrLit pos x)
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
          c <- expr0
          return (TriTree NonTAssign (makeToken a) (makeToken b) (makeToken c))

-- &&  ||
expr0 :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
expr0 =
  (do
    -- &&
    a <- expr0
    meio <- symBoolAndToken
    b <- expr0
    return (TriTree NonTExpr a (makeToken meio) b)
  ) <|> (
  do
    -- ||
    a <- expr0
    meio <- symBoolOrToken
    b <- expr0
    return (TriTree NonTExpr a (makeToken meio) b)
  ) <|> (
  do
    a <- expr1
    return a
  )

-- !
expr1 :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
  expr1 =
    (
    do
      -- !
      a <- symBoolNotToken
      b <- expr1
      return (DualTree NonTExpr (makeToken a) b)
    ) <|> (
    do 
      a <- expr2
      return a
    )

-- <  >  <=  >=  ==  !=
expr2 :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
  expr2 =
    ( 
    -- <
    do 
      a <- expr2
      meio <- symBoolLessThanToken
      b <- expr2
      return (TriTree NonTExpr a (makeToken meio) b)
    ) <|> (
    -- >
    do
      a <- expr2
      meio <- symBoolGreaterThanToken
      b <- expr2
      return (TriTree NonTExpr a (makeToken meio) b)
    ) <|> (
    -- <=
    do
      a <- expr2
      meio <- symBoolLessThanEqToken
      b <- expr2
      return (TriTree NonTExpr a (makeToken meio) b)
    ) <|> (
    -- >=
    do
      a <- expr2
      meio <- symBoolGreaterThanEqToken
      b <- expr2
      return (TriTree NonTExpr a (makeToken meio) b)
    ) <|> (
    -- ==
    do
      a <- expr2
      meio <- symBoolEqToken
      b <- expr2
      return (TriTree NonTExpr a (makeToken meio) b)
    ) <|> (
    -- !=
    do  
      a <- expr2
      meio <- symBoolNotEqToken
      b <- expr2
      return (TriTree NonTExpr a (makeToken meio) b)
    ) <|> (
      do
        a <- expr3
        return a
    )

-- +  -
expr3 :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
  expr3 =
    (
    -- +
    do
      a <- expr3
      meio <- symOpPlusToken
      b <- expr3
      return (TriTree NonTExpr a (makeToken meio) b)
    ) <|> (
    -- -
    do
      a <- expr3
      meio <- symOpMinusToken
      b <- expr3
      return (TriTree NonTExpr a (makeToken meio) b)
    do
    ) <|> (
    do
      a <- expr4
      return a
    )

-- *  /  %
expr4 :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
  expr4 =
    (
    -- *
    do
      a <- expr4
      meio <- symOpMultToken
      b <- expr4
      return (TriTree NonTExpr a (makeToken meio) b)
    ) <|> (
    -- /
    do
      a -> expr4
      meio <- symOpDivToken
      b <- expr4
      return (TriTree NonTExpr a (makeToken meio) b)
    ) <|> (
    -- %
    do
      a -> expr4
      meio <- symOpModToken
      b <- expr4
      return (TriTree NonTExpr a (makeToken meio) b)
    ) <|> (
    do 
      a <- expr5
      return a 
    )

-- ^
expr5 :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
  expr5 =
    (
    -- ^
    do
      a <- expr5
      meio <- symOpExpToken
      b <- expr5
      return (TriTree NonTExpr a (makeToken meio) b)
    ) <|> (
    do
      a <- expr6
      return a
    )

-- function
expr6 :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
  expr6 =
    (
    -- function
    do
      a <- exprFunction
      return (UniTree NonTInvokeFunction a)
    ) <|> (
    do
      a <- expr7
      return a
    )

-- id  literal
expr7 :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
  expr7 =
    (
    -- id
    do
      a <- exprId
      return (UniTree NonTId a)
    ) <|> (
    -- lit
      a <- strLitToken
      return (LeafToken a)
    ) <|> (
    do
      a <- exprFinal
      return a
    )

-- ( )
exprFinal :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
  exprFinal =
    (
    -- ( )
    do
      a <- openParenthToken
      meio <- expr0
      b <- closeParenthToken
      return (UniTree NonTExpr meio)
    )

-- Main e função que chama o parser

parser :: [Token] -> IO (Either ParseError TokenTree)
parser tokens = runParserT program [] "Error message" tokens