{
  module Main (main, Token(..), AlexPosn(..), alexScanTokens, token_posn) where
}

%wrapper "posn"

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters

tokens :-

  $white+                                     ;
  "--".*.                                     ;
  int                                         { \p s -> Int p }
  float                                       { \p s -> Float p }
  string                                      { \p s -> String p }
  $digit+	                                    { \p s -> IntLit p (read s) }
  ";"                                         { \p s -> SemiColon p}
  [\=\+\-\*\/\(\)]			                      { \p s -> Sym p (head s) }
  (\=\=)|(\!\=)|(\<\=)|(\>\=)|(\&\&)|(\|\|)   { \p s -> SymDual p (read s) }
  [\{\}]                                      { \p s -> ScopeSym p }
  if                                          { \p s -> If p }
  $alpha [$alpha $digit \_ \']*	              { \p s -> Var p s }
  \" $alpha [$alpha $digit ! \_ \']* \"       { \p s -> StrLit s}
{
-- Each right-hand side has type :: AlexPosn -> String -> Token
-- Some action helpers:

-- The token type:
data Token =
  Int AlexPosn           |
  Float AlexPosn         |
  String AlexPosn        |
  SemiColon AlexPosn     |
  ScopeSym AlexPosn      |
  If AlexPosn            |  
  IntLit AlexPosn Int    |
  StrLit AlexPosn String |
  Sym AlexPosn Char      |
  Var AlexPosn String    |
  deriving (Eq,Show)

token_posn (In p) = p
token_posn (Sym p _) = p
token_posn (Var p _) = p
token_posn (Int p _) = p

main = do
  s <- getContents
  print (alexScanTokens s)
}