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
  list                                        { \p s -> TypeList p }
  $digit+\.$digit+                            { \p s -> FloatLit p (read s) }
  $digit+                                     { \p s -> IntLit p (read s) }
  ";"                                         { \p s -> Semicolon p}
  ","                                         { \p s -> Comma p}
  \=\=                                      { \p s -> SymBoolEq p }
  \!\=                                      { \p s -> SymBoolNotEq p }
  \<\=                                      { \p s -> SymBoolLessThanEq p }
  \>\=                                      { \p s -> SymBoolGreaterThanEq p }
  \!                                        { \p s -> SymBoolNot p }
  \&\&                                      { \p s -> SymBoolAnd p }
  \|\|                                      { \p s -> SymBoolOr p }
  true                                      { \p s -> SymBoolTrue p }
  false                                     { \p s -> SymBoolFalse p }
  \<                                        { \p s -> SymBoolLessThan p }
  \>                                        { \p s -> SymBoolGreaterThan p }
  \+                                              { \p s -> SymOpPlus p }
  \-                                              { \p s -> SymOpMinus p }
  \*                                              { \p s -> SymOpMult p }
  \/                                              { \p s -> SymOpDiv p }
  \^                                              { \p s -> SymOpExp p }
  \%                                              { \p s -> SymOpMod p }
  \=                                              { \p s -> Attrib p }
  \(                                              { \p s -> OpenParenth p }
  \)                                              { \p s -> CloseParenth p }
  \[                                              { \p s -> OpenBracket p }
  \]                                              { \p s -> CloseBracket p }
  \{                                              { \p s -> OpenScope p }
  \}                                              { \p s -> CloseScope p }
  \=\>                                           { \p s -> SymPtrOp p }
  \$                                             { \p s -> SymAdressOp p }
  print                                         { \p s -> Print p }
  do                                              { \p s -> Do p }
  endfor                                          { \p s -> EndFor p }
  for                                             { \p s -> For p }
  endwhile                                        { \p s -> EndWhile p }
  while                                           { \p s -> While p }
  endif                                           { \p s -> EndIf p }
  if                                              { \p s -> If p }
  then                                            { \p s -> Then p }
  endelse                                         { \p s -> EndElse p }
  else                                            { \p s -> Else p }
  endelif                                         { \p s -> EndElif p }
  elif                                            { \p s -> Elif p }
  procedure                                       { \p s -> ProcedureTok p }
  function                                        { \p s -> FunctionTok p }
  return                                          { \p s -> Return p }
  break                                           { \p s -> Break p }
  continue                                        { \p s -> Continue p }
  new                                             { \p s -> New p }
  delete                                          { \p s -> Delete p }
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
  TypeList AlexPosn        |
  Attrib AlexPosn         |
  OpenParenth AlexPosn |
  CloseParenth AlexPosn |
  OpenBracket AlexPosn |
  CloseBracket AlexPosn |
  OpenScope AlexPosn |
  CloseScope AlexPosn |
  Semicolon AlexPosn     |
  Comma AlexPosn        |
  SymPtrOp AlexPosn     |
  SymAdressOp AlexPosn     |
  Print AlexPosn            |
  Do AlexPosn            | 
  If AlexPosn            |  
  EndIf AlexPosn         |  
  Then AlexPosn         |  
  Else AlexPosn          |
  EndElse AlexPosn       |
  Elif AlexPosn          |
  EndElif AlexPosn       |
  For AlexPosn            |  
  EndFor AlexPosn            |  
  While AlexPosn            |  
  EndWhile AlexPosn            |  
  ProcedureTok AlexPosn               |
  FunctionTok AlexPosn                |
  Return AlexPosn                  |
  Break AlexPosn                   |
  Continue AlexPosn                |
  New AlexPosn                |
  Delete AlexPosn                |
  FloatLit AlexPosn Float |
  IntLit AlexPosn Int     |
  StrLit AlexPosn String  |
  SymOpPlus AlexPosn |
  SymOpMinus AlexPosn |
  SymOpMult AlexPosn |
  SymOpDiv AlexPosn |
  SymOpExp AlexPosn |
  SymOpMod AlexPosn |
  SymBoolEq AlexPosn |
  SymBoolNotEq AlexPosn |
  SymBoolLessThanEq AlexPosn |
  SymBoolGreaterThanEq AlexPosn |
  SymBoolNot AlexPosn |
  SymBoolAnd AlexPosn |
  SymBoolOr AlexPosn |
  SymBoolTrue AlexPosn |
  SymBoolFalse AlexPosn |
  SymBoolLessThan AlexPosn |
  SymBoolGreaterThan AlexPosn |
  Id AlexPosn String    
  deriving (Eq,Show)

getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}
}
