module Main (main) where

import Lexico
import Text.Parsec
import Control.Monad.IO.Class

import System.IO.Unsafe

data TokenTree = NonTToken NonTToken TokenTree TokenTree TokenTree TokenTree | Token Token | None deriving (Eq, Show)
data NonTToken = 
  NonTProgram |
  NonTStatements |
  NonTStatement |
  NonTAssign |
  NonTIf
  deriving (Eq, Show)

makeTree :: NonTToken -> TokenTree -> TokenTree -> TokenTree -> TokenTree -> TokenTree 
makeTree tok a b c d = NonTToken tok a b c d

makeToken :: Token -> TokenTree
makeToken tok = Token tok

-- parsers para os tokens
typeIntToken :: ParsecT [Token] st IO (Token)
typeIntToken = tokenPrim show update_pos get_token where
  get_token (TypeInt pos) = Just (TypeInt pos)
  get_token _             = Nothing

typeBooleanToken :: ParsecT [Token] st IO (Token)
typeBooleanToken = tokenPrim show update_pos get_token where
  get_token (TypeBoolean pos) = Just (TypeBoolean pos)
  get_token _                 = Nothing

semicolonToken = tokenPrim show update_pos get_token where
  get_token (Semicolon pos) = Just (Semicolon pos)
  get_token _               = Nothing

attribToken = tokenPrim show update_pos get_token where
  get_token (Attrib pos) = Just (Attrib pos)
  get_token _            = Nothing

varToken = tokenPrim show update_pos get_token where
  get_token (Var pos x) = Just (Var pos x)
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
            return (makeTree NonTProgram b None None None)

stmts :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
stmts = (do a <- stmt
            b <- stmts
            return (makeTree NonTStatements a b None None)) <|> (return None)

stmt :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
stmt = do
       first <- assign
       colon <- semicolonToken
       return (makeTree NonTStatement first None None None)

assign :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
assign = do
          a <- varToken
          b <- attribToken
          c <- strLitToken
          return (makeTree NonTAssign (makeToken a) (makeToken b) (makeToken c) None)

-- Main e função que chama o parser

parser :: [Token] -> IO (Either ParseError TokenTree)
parser tokens = runParserT program [] "Error message" tokens

main :: IO ()
main = case unsafePerformIO (parser (getTokens "arquivo.in")) of
            { Left err -> print err; 
              Right ans -> print ans
            }