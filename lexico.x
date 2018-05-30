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
  \=\=                                      { \p s -> SymBoolEq p s }
  \!\=                                      { \p s -> SymBoolNotEq p s }
  \<\=                                      { \p s -> SymBoolLessThanEq p s }
  \>\=                                      { \p s -> SymBoolGreaterThanEq p s }
  \&\&                                      { \p s -> SymBoolAnd p s }
  \|\|                                      { \p s -> SymBoolOr p s }
  \<                                        { \p s -> SymBoolLessThan p s }
  \>                                        { \p s -> SymBoolGreaterThan p s }
  \+                                              { \p s -> SymOpPlus p (head s) }
  \-                                              { \p s -> SymOpMinus p (head s) }
  \*                                              { \p s -> SymOpMult p (head s) }
  \/                                              { \p s -> SymOpDiv p (head s) }
  \^                                              { \p s -> SymOpExp p (head s) }
  \=                                              { \p s -> Attrib p }
  \(                                              { \p s -> OpenParenth p }
  \)                                              { \p s -> CloseParenth p }
  \[                                              { \p s -> OpenBracket p }
  \]                                              { \p s -> CloseBracket p }
  \{                                              { \p s -> OpenScope p }
  \}                                              { \p s -> CloseScope p }
  \=\>                                          { \p s -> SymPtrOp p }
  \$                                          { \p s -> SymAdressOp p }
  endfor                                          { \p s -> EndFor p }
  for                                          { \p s -> For p }
  endwhile                                          { \p s -> EndWhile p }
  while                                          { \p s -> While p }
  endif                                          { \p s -> EndIf p }
  if                                          { \p s -> If p }
  $alpha [$alpha $digit \_ \']*               { \p s -> Id p s }
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
  SymPtrOp AlexPosn     |
  SymAdressOp AlexPosn     |
  If AlexPosn            |  
  EndIf AlexPosn            |  
  For AlexPosn            |  
  EndFor AlexPosn            |  
  While AlexPosn            |  
  EndWhile AlexPosn            |  
  IntLit AlexPosn Int    |
  StrLit AlexPosn String |
  SymOpPlus AlexPosn Char |
  SymOpMinus AlexPosn Char |
  SymOpMult AlexPosn Char |
  SymOpDiv AlexPosn Char |
  SymOpExp AlexPosn Char |
  SymBoolEq AlexPosn String |
  SymBoolNotEq AlexPosn String |
  SymBoolLessThanEq AlexPosn String |
  SymBoolGreaterThanEq AlexPosn String |
  SymBoolAnd AlexPosn String |
  SymBoolOr AlexPosn String |
  SymBoolLessThan AlexPosn String |
  SymBoolGreaterThan AlexPosn String |
  Id AlexPosn String    
  deriving (Eq,Show)

getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}
}
