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
          c <- strLitToken
          return (TriTree NonTAssign (makeToken a) (makeToken b) (makeToken c))

-- Main e função que chama o parser

parser :: [Token] -> IO (Either ParseError TokenTree)
parser tokens = runParserT program [] "Error message" tokens