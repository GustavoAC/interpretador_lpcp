-- Comente para executar o main local
-- module Sintatico (TokenTree(..), NonTToken(..), parser) where


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
  NonTDelPtr |
  NonTStructDecl |
  NonTStructDecls |
  NonTFuncDecls |
  NonTFuncDecl |
  NonTProcDecl |
  NonTVarDecls |
  NonTAssign |
  NonTPointTo |
  NonTIf |
  NonTElse |
  NonTElif |
  NonTWhile |
  NonTExpr |
  NonTInvokeFunction |
  NonTId |
  NonTFor|
  NonTInvokeFunctionArgs |
  NonTCallProcedure      |
  NonTReturn             |
  NonTPtrOp              |
  NonTArray              |
  NonTParams             |
  NonTListIndex          |
  NonTIndex              |
  NonTDecl               |
  NonTPrint              |
  NonTAccessStruct       |
  NonTScan               |
  NonTListIds            |
  NonTPtrType            |
  NonTListType           |
  NonTStructType deriving (Eq, Show)

makeToken :: Token -> TokenTree
makeToken tok = LeafToken tok

-- parsers para os tokens

debugToken :: ParsecT [Token] st IO (Token)
debugToken = tokenPrim show update_pos get_token where
  get_token (DebugTok pos) = Just (DebugTok pos)
  get_token _              = Nothing

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

typeListToken :: ParsecT [Token] st IO (Token)
typeListToken = tokenPrim show update_pos get_token where
  get_token (TypeList pos) = Just (TypeList pos)
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

symOpPlusPlusToken :: ParsecT [Token] st IO (Token)
symOpPlusPlusToken = tokenPrim show update_pos get_token where
  get_token (SymOpPlusPlus pos) = Just (SymOpPlusPlus pos)
  get_token _                   = Nothing

symOpMinusMinusToken :: ParsecT [Token] st IO (Token)
symOpMinusMinusToken = tokenPrim show update_pos get_token where
  get_token (SymOpMinusMinus pos) = Just (SymOpMinusMinus pos)
  get_token _                     = Nothing

symOpPlusAssignToken :: ParsecT [Token] st IO (Token)
symOpPlusAssignToken = tokenPrim show update_pos get_token where
  get_token (SymOpPlusAssign pos) = Just (SymOpPlusAssign pos)
  get_token _                     = Nothing

symOpMinusAssignToken :: ParsecT [Token] st IO (Token)
symOpMinusAssignToken = tokenPrim show update_pos get_token where
  get_token (SymOpMinusAssign pos) = Just (SymOpMinusAssign pos)
  get_token _                      = Nothing

symOpMultAssignToken :: ParsecT [Token] st IO (Token)
symOpMultAssignToken = tokenPrim show update_pos get_token where
  get_token (SymOpMultAssign pos) = Just (SymOpMultAssign pos)
  get_token _                     = Nothing

symOpDivAssignToken :: ParsecT [Token] st IO (Token)
symOpDivAssignToken = tokenPrim show update_pos get_token where
  get_token (SymOpDivAssign pos) = Just (SymOpDivAssign pos)
  get_token _                    = Nothing

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
doToken :: ParsecT [Token] st IO (Token)
doToken = tokenPrim show update_pos get_token where
  get_token (Do pos)  = Just (Do pos)
  get_token _         = Nothing

forToken :: ParsecT [Token] st IO (Token)
forToken = tokenPrim show update_pos get_token where
  get_token (For pos) = Just (For pos)
  get_token _         = Nothing

endForToken :: ParsecT [Token] st IO (Token)
endForToken = tokenPrim show update_pos get_token where
  get_token (EndFor pos) = Just (EndFor pos)
  get_token _            = Nothing

foreachToken :: ParsecT [Token] st IO (Token)
foreachToken = tokenPrim show update_pos get_token where
  get_token (Foreach pos) = Just (Foreach pos)
  get_token _         = Nothing

endForeachToken :: ParsecT [Token] st IO (Token)
endForeachToken = tokenPrim show update_pos get_token where
  get_token (EndForeach pos) = Just (EndForeach pos)
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

thenToken :: ParsecT [Token] st IO (Token)
thenToken = tokenPrim show update_pos get_token where
  get_token (Then pos) = Just (Then pos)
  get_token _           = Nothing

elseToken :: ParsecT [Token] st IO (Token)
elseToken = tokenPrim show update_pos get_token where
  get_token (Else pos) = Just (Else pos)
  get_token _           = Nothing

endElseToken :: ParsecT [Token] st IO (Token)
endElseToken = tokenPrim show update_pos get_token where
  get_token (EndElse pos) = Just (EndElse pos)
  get_token _              = Nothing

elifToken :: ParsecT [Token] st IO (Token)
elifToken = tokenPrim show update_pos get_token where
  get_token (Elif pos) = Just (Elif pos)
  get_token _           = Nothing

endElifToken :: ParsecT [Token] st IO (Token)
endElifToken = tokenPrim show update_pos get_token where
  get_token (EndElif pos) = Just (EndElif pos)
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
colonToken :: ParsecT [Token] st IO (Token)
colonToken = tokenPrim show update_pos get_token where
  get_token (Colon pos) = Just (Colon pos)
  get_token _               = Nothing

semicolonToken :: ParsecT [Token] st IO (Token)
semicolonToken = tokenPrim show update_pos get_token where
  get_token (Semicolon pos) = Just (Semicolon pos)
  get_token _               = Nothing

commaToken :: ParsecT [Token] st IO (Token)
commaToken = tokenPrim show update_pos get_token where
  get_token (Comma pos) = Just (Comma pos)
  get_token _               = Nothing

endPointToken :: ParsecT [Token] st IO (Token)
endPointToken = tokenPrim show update_pos get_token where
  get_token (EndPoint pos) = Just (EndPoint pos)
  get_token _               = Nothing

attribToken :: ParsecT [Token] st IO (Token)
attribToken = tokenPrim show update_pos get_token where
  get_token (Attrib pos) = Just (Attrib pos)
  get_token _            = Nothing

printToken :: ParsecT [Token] st IO (Token)
printToken = tokenPrim show update_pos get_token where
  get_token (Print pos)  = Just (Print pos)
  get_token _            = Nothing

scanToken :: ParsecT [Token] st IO (Token)
scanToken = tokenPrim show update_pos get_token where
  get_token (Scan pos)  = Just (Scan pos)
  get_token _            = Nothing

procedureToken :: ParsecT [Token] st IO (Token)
procedureToken = tokenPrim show update_pos get_token where
  get_token (ProcedureTok pos) = Just (ProcedureTok pos)
  get_token _               = Nothing

functionToken :: ParsecT [Token] st IO (Token)
functionToken = tokenPrim show update_pos get_token where
  get_token (FunctionTok pos) = Just (FunctionTok pos)
  get_token _              = Nothing

newToken :: ParsecT [Token] st IO (Token)
newToken = tokenPrim show update_pos get_token where
  get_token (New pos) = Just (New pos)
  get_token _            = Nothing

deleteToken :: ParsecT [Token] st IO (Token)
deleteToken = tokenPrim show update_pos get_token where
  get_token (Delete pos)  = Just (Delete pos)
  get_token _            = Nothing

structToken :: ParsecT [Token] st IO (Token)
structToken = tokenPrim show update_pos get_token where
  get_token (StructTok pos)  = Just (StructTok pos)
  get_token _                = Nothing


-- O que ele quis dizer com isso?
update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos 

-- Parsers não terminais

--         Parsec   input       state         output
program :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
program = try (
  do
    a <- structDecls
    b <- funcDecls
    c <- stmts
    return (TriTree NonTProgram a b c)
  ) <|> try (
  do
    b <- funcDecls
    c <- stmts
    eof
    return (DualTree NonTProgram b c)
  ) <|> try (
  do
    a <- structDecls
    c <- stmts
    return (DualTree NonTProgram a c)
  ) <|> try (
  do
    c <- stmts
    eof
    return (UniTree NonTProgram c)
  )
  

stmts :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
stmts = try (
  do
    a <- singleStmt
    b <- stmts
    return (DualTree NonTStatements a b)
  ) <|> try (
  do
    a <- singleStmt
    return (UniTree NonTStatement a)
  )

singleStmt :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
singleStmt = try (
  -- Fluxo
  do
   first <- jumpStmt
   colon <- semicolonToken
   return first
  ) <|> try (
  -- Controle
  do
    first <- controlStmt
    return first
  ) <|> try (
  -- Basico
  do
   first <- basicStmt
   colon <- semicolonToken
   return first
  )

jumpStmt :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
jumpStmt = try (
  do
    ret <- returnToken
    val <- expr0;
    return (UniTree NonTReturn val);
  ) <|> try (
  do
    ret <- returnToken
    return (UniTree NonTReturn None);
  )

controlStmt :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
controlStmt = try (
  -- Loops
  do
    first <- loop
    return first
  ) <|> try (
  -- Condicionais
  do
    first <- condition
    return first
  )
 
basicStmt :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
basicStmt = try (
  -- print
  do 
    first <- printToken
    things <- listParam
    return (UniTree NonTPrint things)
  ) <|> try (
  -- Declarações
  do
   first <- decl
   return first
  ) <|> try (
  -- Atribuição
  do
   first <- assign
   return first
  ) <|> try (
  -- ptr apontar : ponteiro => new int;
  do
    first <- point_to
    return first
  ) <|> try (
  -- delete ponteiro 
  do 
    token <- deleteToken
    id    <- exprId
    return (UniTree NonTDelPtr (UniTree NonTId id) )
  ) <|> try (
  -- procedimento
  do 
    a <- callProcedure
    return a
  ) <|> try (
  -- debug
  do 
    a <- debugToken
    return (makeToken a)
  )

decl :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
decl = try (
  do 
    type_token <- types
    id <- listIds
    return ( DualTree NonTDecl type_token id)  
  )

funcDecls :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
funcDecls = try (
  -- <funcDec> <funcDecls>
  do
    f_decl  <- funcDecl
    f_decls <- funcDecls
    return (DualTree NonTFuncDecls f_decl f_decls)
  ) <|> try (
  -- <procDec> <funcDecls>
  do
    p_decl  <- procDec
    f_decls <- funcDecls
    return (DualTree NonTFuncDecls p_decl f_decls)
  ) <|> try (
  -- <funcDec>
  do
    f_decl  <- funcDecl
    return (UniTree NonTFuncDecls f_decl)
  ) <|> try (
  -- <procDec
  do
    p_decl  <- procDec
    return (UniTree NonTFuncDecls p_decl)
  )

structDecls :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
structDecls = try (
  -- <structDecls> <structDecls>
  do
    s_decl  <- structDecl
    s_decls <- structDecls
    return (DualTree NonTStructDecls s_decl s_decls)
  ) <|> try (
  -- <structDecls>
  do
    s_decl  <- structDecl
    return (UniTree NonTStructDecls s_decl)
  )

structDecl :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
structDecl = try (
  -- struct <id> { <listDecls> }
  do
    token <- structToken
    id    <- idToken
    s1    <- openScopeToken
    decls <- varDecls 
    finSC <- semicolonToken
    s2    <- closeScopeToken
    return (DualTree NonTStructDecl (makeToken id) decls)
  )

funcDecl :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
funcDecl = try (
  -- function <id> (<varDecls>) : <Type> { <stmts> }
  do
    token  <- functionToken
    id     <- idToken
    p1     <- openParenthToken
    list_p <- varDecls
    p2     <- closeParenthToken
    two_p  <- colonToken
    t      <- types
    s1     <- openScopeToken
    stmts  <- stmts
    s2     <- closeScopeToken
    return (QuadTree NonTFuncDecl (makeToken id) list_p t stmts)
  ) <|> try (
  -- function <id> () : <Type> { <stmts> }
  do
    token  <- functionToken
    id     <- idToken
    p1     <- openParenthToken
    p2     <- closeParenthToken
    two_p  <- colonToken
    t      <- types
    s1     <- openScopeToken
    stmts  <- stmts
    s2     <- closeScopeToken
    return (QuadTree NonTFuncDecl (makeToken id) None t stmts)
  )

procDec :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
procDec = try (
  -- procedure <id> (<varDecls>) { <stmts> }
  do
    token  <- procedureToken
    id     <- idToken
    p1     <- openParenthToken
    list_p <- varDecls
    p2     <- closeParenthToken
    s1     <- openScopeToken
    stmts  <- stmts
    s2     <- closeScopeToken
    return (TriTree NonTProcDecl (makeToken id) list_p stmts)
  ) <|> try (
  -- procedure <id> () { <stmts> }
  do
    token  <- procedureToken
    id     <- idToken
    p1     <- openParenthToken
    p2     <- closeParenthToken
    s1     <- openScopeToken
    stmts  <- stmts
    s2     <- closeScopeToken
    return (TriTree NonTProcDecl (makeToken id) None stmts)
  )

varDecls :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
varDecls = try (
  -- tipo id, ..., idn ; tipo id, id2, ..., idm; tipo...
  do
    t        <- types
    list_ids <- pureListIds
    p        <- semicolonToken
    v        <- varDecls
    return (TriTree NonTVarDecls t list_ids v)
  ) <|> try (
  -- tipo id, ..., idn
  do
    t        <- types
    list_ids <- pureListIds
    return (DualTree NonTVarDecls t list_ids)
  )

listIds :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
listIds = try (
  -- a => algo, ...
  do 
    id <- point_to
    v <- commaToken
    list <- listIds
    return (DualTree NonTListIds id list)
  ) <|> try (
  -- a => algo
  do 
    a <- point_to
    return a
  ) <|> try (
  -- a = algo, ...
  do 
    id <- assign
    v <- commaToken
    list <- listIds
    return (DualTree NonTListIds id list)
  ) <|> try (
  -- a = algo
  do 
    a <- assign
    return a
  ) <|> try (
  -- a, ...
  do 
    id <- idToken
    v <- commaToken
    list <- listIds
    return (DualTree NonTListIds (makeToken id) list)
  ) <|> try (
  -- a
  do
    id <- idToken
    return (LeafToken id)
  )

pureListIds :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
pureListIds = try (
  -- a, ...
  do 
    id <- idToken
    v <- commaToken
    list <- pureListIds
    return (DualTree NonTListIds (makeToken id) list)
  ) <|> try (
  -- a
  do
    id <- idToken
    return (LeafToken id)
  )

types :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
types = try (
    do
      t <- openBracketToken
      t2 <- types
      t3 <- closeBracketToken
      return (UniTree NonTListType t2)
    ) <|> try (
    do
      t <- typePointerToken
      t2 <- types
      return (UniTree NonTPtrType t2)
    ) <|> try (
    do
      t <- typeIntToken
      return (LeafToken t)
    ) <|> try (
    do 
      t <- typeFloatToken
      return (LeafToken t)
    ) <|> try (
    do 
      t <- typeStringToken
      return (LeafToken t)
    ) <|> try (
    do
      t <- typeBooleanToken
      return (LeafToken t)
    ) <|> (
    do
      id <- idToken
      return (UniTree NonTStructType (makeToken id))
    )

assign :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
assign = do
            a <- exprId
            b <- attribToken
            c <- rightHandAssign
            return (DualTree NonTAssign (UniTree NonTId a) c)
  

rightHandAssign :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
rightHandAssign = try (
    do
      a <- expr0
      return a
  ) <|> try (
    do
      a <- scanToken
      b <- scanType
      return (UniTree NonTScan b)
  )

scanType :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
scanType = try (
    do
      a <- typeIntToken
      return (LeafToken a)
  ) <|> try (
    do
      a <- typeFloatToken
      return (LeafToken a)
  ) <|> try (
    do
      a <- typeStringToken
      return (LeafToken a)
  ) <|> try (
    do
      a <- typeBooleanToken
      return (LeafToken a)
  )

point_to :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
point_to = try (
  -- id => new tipo
  do
    a <- exprId
    b <- symPtrOpToken
    c <- newToken
    d <- types
    return (DualTree NonTPointTo (UniTree NonTId a) d)
  ) <|> try (
  -- id => id
  do
    a <- exprId
    b <- symPtrOpToken
    c <- idToken
    return (DualTree NonTPointTo (UniTree NonTId a) (makeToken c))
  ) 

loop :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
loop = try (
  do
    while <- whileLoop
    return while
  ) <|> (
  do 
    for <- forLoop
    return for 
  )
  -- to do acrescentar for futuramente

condition :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
condition = try (
  -- if(<expr>) then <stmts> endif ...
  do
    ifsymb <- ifToken
    p1 <- openParenthToken
    e <- expr0
    p2 <- closeParenthToken
    d <- thenToken
    s <- stmts
    endifsymb <- endIfToken
    cond2 <- condition2 
    return (TriTree NonTIf e s cond2)
  ) <|> try (
  -- if(<expr>) then <stmts> endif
  do
    ifsymb <- ifToken
    p1 <- openParenthToken
    e <- expr0
    p2 <- closeParenthToken
    d <- thenToken
    s <- stmts
    endifsymb <- endIfToken
    return (TriTree NonTIf e s None)
  )

condition2 :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
condition2 = try (
  -- elif (<expr>) then <stmts> endelif <condition2>
  do
    elifsymb <- elifToken
    p1 <- openParenthToken
    e <- expr0
    p2 <- closeParenthToken
    d <- thenToken
    s <- stmts
    endelifsymb <- endElifToken
    cond2 <- condition2 
    return (TriTree NonTIf e s cond2)
  ) <|> try (
  -- elif (<expr>) then <stmts> endelif
  do
    elifsymb <- elifToken
    p1 <- openParenthToken
    e <- expr0
    p2 <- closeParenthToken
    d <- thenToken
    s <- stmts
    endelifsymb <- endElifToken
    return (TriTree NonTIf e s None)
  ) <|> try (
  -- else then <stmts> endelse
  do
    elsesymb <- elseToken
    d <- thenToken
    s <- stmts
    endelsesymb <- endElseToken
    return (UniTree NonTElse s)
  )

whileLoop :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
whileLoop = try (
  do
    a <- whileToken
    b <- openParenthToken
    c <- expr0
    d <- closeParenthToken
    e <- doToken
    e <- stmts
    f <- endWhileToken
    return (DualTree NonTWhile c e)
  )

-- Não testado o for
forLoop :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
forLoop = try (
  do
    a <- forToken
    b <- openParenthToken
    c <- basicStmt
    d <- semicolonToken
    e <- expr0
    f <- semicolonToken
    g <- basicStmt
    h <- closeParenthToken
    i <- stmts
    j <- endForToken
    return (QuadTree NonTFor (UniTree NonTId c) e g i)
  )

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
expr3Ops = (
  do
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
expr4Ops = (
  do
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
expr5Ops = (
  do
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
    return a
  ) <|> try (
  -- true lit
  do 
    a <- symBoolTrueToken
    return (LeafToken a)
  ) <|> (
  -- false lit
  do
    a <- symBoolFalseToken
    return (LeafToken a)
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
  ) <|> try (
  -- intlit
  do
    a <- intLitToken
    return (LeafToken a)
  )

-- Chamada de procedimento
callProcedure :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
callProcedure = try (
  -- nomeProcedimento(a, b, ...) 
  do
    name <- idToken
    a <- openParenthToken
    b <- listParam
    c <- closeParenthToken
    return (DualTree NonTCallProcedure (makeToken name) b) -- ?
  ) <|> (
  -- nomeProcedimento()
  do
    name <- idToken
    a <- openParenthToken
    b <- closeParenthToken
    return (DualTree NonTCallProcedure (makeToken name) None) -- ?
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
    return (UniTree NonTInvokeFunction (makeToken name)) -- ?
  )

listParam :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
listParam = try (
  -- param, ... , param
  do
    a <- expr0
    b <- commaToken
    c <- listParam
    return (DualTree NonTParams a c) -- ?
  ) <|> (
  -- param
  do 
    a <- expr0
    return (UniTree NonTParams a)
  )

exprId :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
exprId =  try (
  -- $(algo)[]
  do 
    a <- symAdressOpToken
    b <- openParenthToken
    c <- exprId
    d <- closeParenthToken
    e <- listIndexes
    return (DualTree NonTArray (UniTree NonTPtrOp c) e) -- ?
  ) <|> try (
  -- $( algo )
  do
    a <- symAdressOpToken
    b <- openParenthToken
    c <- exprId
    d <- closeParenthToken
    return (UniTree NonTPtrOp c)
  ) <|> try ( 
  -- $a 
  do 
    a <- symAdressOpToken
    b <- exprId
    return (UniTree NonTPtrOp b) -- ?
  ) <|> try (
  -- a[] 
  do 
    a <- idToken
    b <- listIndexes
    return (DualTree NonTArray (makeToken a) b) -- ?
  ) <|> try (
  -- (exprId).prop
  do 
    p1 <- openParenthToken
    a  <- exprId
    p2 <- closeParenthToken
    b  <- endPointToken
    c  <- exprId
    return (DualTree NonTAccessStruct a c)
  ) <|> try (
  -- id.prop
  do 
    a <- idToken
    b <- endPointToken
    c <- exprId
    return (DualTree NonTAccessStruct (makeToken a) c)
  ) <|> try (
  -- a
  do 
    a <- idToken
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
    return ( DualTree NonTListIndex b d) -- ?
  ) <|> (
  -- [x]
  do
    a <- openBracketToken
    b <- expr0
    c <- closeBracketToken
    return ( UniTree NonTIndex b)
  )

-- Main e função que chama o parser

parser :: [Token] -> IO (Either ParseError TokenTree)
parser tokens = runParserT program [] "Error message" tokens

{--
  Descomente para usar o main local
--}
main :: IO ()
main = case unsafePerformIO (parser (getTokens "arquivo.in")) of
              { Left err -> print err; 
                Right ans -> print ans
              }
