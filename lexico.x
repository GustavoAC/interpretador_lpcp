{
module Lexico (getTokens, Token(..), AlexPosn(..), alexScanTokens) where

import System.IO
import System.IO.Unsafe
}

%wrapper "posn"

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters

tokens :-

  $white+                                     ;
  "--".*.                                     ;
  int                                         { \p s -> TypeInt p }
  float                                       { \p s -> TypeFloat p }
  string                                      { \p s -> TypeString p }
  ptr                                         { \p s -> TypePointer p }
  bool                                        { \p s -> TypeBoolean p }
  $digit+                                     { \p s -> IntLit p (read s) }
  ";"                                         { \p s -> Semicolon p}
  (\=\=)|(\!\=)|(\<\=)|(\>\=)|(\&\&)|(\|\|)|(\<)|(\>)   { \p s -> BoolSym p s }
  [\+\-\*\/\^]                            { \p s -> Sym p (head s) }
  \=                                              { \p s -> Attrib p }
  \(                                              { \p s -> OpenParenth p }
  \)                                              { \p s -> CloseParenth p }
  \[                                              { \p s -> OpenBracket p }
  \]                                              { \p s -> CloseBracket p }
  \{                                              { \p s -> OpenScope p }
  \}                                              { \p s -> CloseScope p }
  \=\>                                          { \p s -> PtrOp p }
  \$                                          { \p s -> AdressOp p }
  endfor                                          { \p s -> EndFor p }
  for                                          { \p s -> For p }
  endwhile                                          { \p s -> EndWhile p }
  while                                          { \p s -> While p }
  endif                                          { \p s -> EndIf p }
  if                                          { \p s -> If p }
  $alpha [$alpha $digit \_ \']*               { \p s -> Var p s }
  \" $alpha [$alpha $digit ! \_ \']* \"       { \p s -> StrLit p s }
{
-- Each right-hand side has type :: AlexPosn -> String -> Token
-- Some action helpers:

-- The token type:
data Token =
  TypeInt AlexPosn           |
  TypeFloat AlexPosn         |
  TypeString AlexPosn        |
  TypePointer AlexPosn        |
  TypeBoolean AlexPosn        |
  Attrib AlexPosn         |
  OpenParenth AlexPosn |
  CloseParenth AlexPosn |
  OpenBracket AlexPosn |
  CloseBracket AlexPosn |
  OpenScope AlexPosn |
  CloseScope AlexPosn |
  Semicolon AlexPosn     |
  PtrOp AlexPosn     |
  AdressOp AlexPosn     |
  If AlexPosn            |  
  EndIf AlexPosn            |  
  For AlexPosn            |  
  EndFor AlexPosn            |  
  While AlexPosn            |  
  EndWhile AlexPosn            |  
  IntLit AlexPosn Int    |
  StrLit AlexPosn String |
  Sym AlexPosn Char      |
  BoolSym AlexPosn String |
  Var AlexPosn String    
  deriving (Eq,Show)

getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}
}
