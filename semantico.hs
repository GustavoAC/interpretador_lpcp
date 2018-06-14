import Sintatico
import Lexico

import System.IO.Unsafe


-- ListaDeStructs(nome [campos (nome typo)]) ListaDeProcedimentos(Id Parâmetros BlocoDaFunção) ListaDeFunções(Id TipodeRetorno Parâmetros BlocoDaFunção) ListadeEscopos ListaDeVariáveis(Id Tipo Valor Escopo)
data State = State SymbolTable (IO ())
--                             structs  procedimentos funções escAtuais memoria
data SymbolTable = SymbolTable [Struct] [Procedure] [Function] [Scope] Memory deriving (Eq, Show)
--                          nome   campos   bloco
data Procedure = Procedure String [Field] TokenTree deriving (Eq, Show)
--                        nome retorno campos bloco
data Function = Function String Type [Field] TokenTree deriving (Eq, Show)
--                    nome  campos
data Struct = Struct String [Field] deriving (Eq, Show)
--                 (nome e profundidade do subprograma) (nome e profundidade do subbloco)
data Scope = Scope (String, Int) (String, Int) deriving (Eq, Show)
--           campo tipo  nome
data Field = Field Type String deriving (Show)
-- Basta que os tipos sejam iguais para um field ser igual a outro.
instance Eq Field where
    (Field name1 type1) == (Field name2 type2) = (type1 == type2)
--                 listaDeVariaveis
data Memory = Memory [Variable] deriving (Eq, Show)
--                         id   tipo valor escopo
data Variable = Variable String Type Value Scope deriving (Eq, Show)

data Type = IntType | FloatType | StringType | BoolType | ListType Type | PointerType Type | StructType String deriving (Eq, Show)
data Value = Int Int |
             Float Float |
             String String |
             Bool Bool |
             List [Value] |
--                     id   escopo
             Pointer String Scope |
             StructVal [FieldInstance] deriving (Eq, Show)
data FieldInstance = FieldInstance Field Value deriving (Eq, Show)

emptyState :: State
emptyState = State (SymbolTable [] [] [] [(Scope ("main", 1) ("main", 1))] (Memory [])) (return ())

inicAnalisadorSemantico :: TokenTree -> IO()
inicAnalisadorSemantico tree = getAnalisadorIO (analisadorSemantico tree emptyState)

getAnalisadorIO :: State -> IO()
getAnalisadorIO (State _ io) = io

analisadorSemantico :: TokenTree -> State -> State
-- printando a tabela ao fim da execucao
analisadorSemantico (UniTree NonTProgram a) (State table io) = 
    State table2 ((print table2) >> io2)
    where
        (State table2 io2) = analisadorSemantico a (State table io)

-- decl
analisadorSemantico (DualTree NonTDecl declType ids) st = declareMany st (parseType declType) ids
-- assign
analisadorSemantico (DualTree NonTAssign a c) st = assignToId st a c
-- print
analisadorSemantico (UniTree NonTPrint params) st = printAll st params
-- point to
-- analisadorSemantico (LeafToken Continue) (State table io) = error "não implementado"
-- for
-- analisadorSemantico (TriTree NonTFor a b c) (State table io) = error "não implementado"
-- while
analisadorSemantico (DualTree NonTWhile a b) st = resolveWhile st a b
-- if 
analisadorSemantico (TriTree NonTIf a b c) st = resolveIfCondition st a b c
-- procNoArgs
analisadorSemantico (UniTree NonTCallProcedure a) (State table io) = error "não implementado"
-- procArgs
analisadorSemantico (DualTree NonTCallProcedureArgs a b) (State table io) = error "não implementado"
-- break
-- analisadorSemantico (LeafToken Break) (State table io) = error "não implementado"
-- return
-- analisadorSemantico (UniTree NonTReturn b) (State table io) = error "não implementado"
-- continue
-- analisadorSemantico (LeafToken Continue) (State table io) = error "não implementado"

-- -- repetidores genericos
analisadorSemantico (QuadTree _ a b c d) st = 
    analisadorSemantico d st3
    where
        st3 = analisadorSemantico c st2
        st2 = analisadorSemantico b st1
        st1 = analisadorSemantico a st
analisadorSemantico (TriTree _ a b c) st =
    analisadorSemantico c st2
    where
        st2 = analisadorSemantico b st1
        st1 = analisadorSemantico a st
-- NonTStatements cai aqui
analisadorSemantico (DualTree _ a b) st =
    analisadorSemantico b st1
    where
       st1 = analisadorSemantico a st
-- NonTStatement cai aqui
analisadorSemantico (UniTree _ a) st = 
    analisadorSemantico a st
analisadorSemantico (None) st = st

-- resta declarações com assigns
declareMany :: State -> Type -> TokenTree -> State
declareMany st typ (DualTree NonTListIds (LeafToken (Id _ id)) rest) =
    declareMany finalSt typ rest
    where
        (State (SymbolTable a b c (currScope:scopes) (Memory mem)) io) = st
        newVar = Variable id typ (defaultVal typ) currScope
        finalMem = instanciarVar mem newVar
        finalSt = (State (SymbolTable a b c (currScope:scopes) (Memory finalMem)) io)
declareMany st typ (DualTree NonTListIds (DualTree NonTAssign (UniTree NonTId (LeafToken (Id _ id))) expr) rest) =
    declareMany finalSt typ rest
    where
        (st1, (exprTyp, exprVal)) = avaliarExpressao st expr
        (State (SymbolTable a b c (currScope:scopes) (Memory mem)) io) = st1
        finalMem = declareWithVal mem id typ exprTyp exprVal currScope
        finalSt = (State (SymbolTable a b c (currScope:scopes) (Memory finalMem)) io)
declareMany st typ (LeafToken (Id _ id)) = finalSt
    where
        (State (SymbolTable a b c (currScope:scopes) (Memory mem)) io) = st
        newVar = Variable id typ (defaultVal typ) currScope
        finalMem = instanciarVar mem newVar
        finalSt = (State (SymbolTable a b c (currScope:scopes) (Memory finalMem)) io)
declareMany st typ (DualTree NonTAssign (UniTree NonTId (LeafToken (Id _ id))) expr) = finalSt
    where
        (st1, (exprTyp, exprVal)) = avaliarExpressao st expr
        (State (SymbolTable a b c (currScope:scopes) (Memory mem)) io) = st1
        finalMem = declareWithVal mem id typ exprTyp exprVal currScope
        finalSt = (State (SymbolTable a b c (currScope:scopes) (Memory finalMem)) io)
        

declareWithVal :: [Variable] -> String -> Type -> Type -> Value -> Scope -> [Variable]
declareWithVal mem id expectedType realType val currScope =
    if expectedType == realType then
        instanciarVar mem (Variable id realType val currScope)
    else error "Erro na declaração: Tipo da expressão não corresponde ao tipo declarado"

defaultVal :: Type -> Value
defaultVal IntType = Int 0
defaultVal FloatType = Float 0.0
defaultVal StringType = String []
defaultVal BoolType = Bool False
defaultVal (PointerType _) = Pointer [] (Scope ([],0) ([],0))
defaultVal (ListType _) = List []
defaultVal (StructType _) = StructVal []

parseType :: TokenTree -> Type
parseType (UniTree NonTListType a) = ListType (parseType a)
parseType (UniTree NonTPtrType a) = PointerType (parseType a)
parseType (UniTree NonTStructType (LeafToken (Id _ id))) = StructType id
parseType (LeafToken (TypeInt _)) = IntType
parseType (LeafToken (TypeFloat _)) = FloatType
parseType (LeafToken (TypeString _)) = StringType
parseType (LeafToken (TypeBoolean _)) = BoolType
parseType _ = error "Erro inesperado em parseType"

assignToId :: State -> TokenTree -> TokenTree -> State
assignToId st id expr = finalState
    where
        (st1, exprRes) = avaliarExpressao st expr
        (st2, finalVal) = criarValorParaAtribuicao st1 id exprRes
        (State (SymbolTable a b c d (Memory mem)) io) = st2
        finalMem = atribuirVar mem finalVal
        finalState = (State (SymbolTable a b c d (Memory finalMem)) io)

-- TODO: TESTAR FAMÍLIA DE FUNÇÕES STARTPROCEDURE
--       Trocar params de [String] pra [(Type, Value)] e avaliar por aqui
--       Se cada valor passado é um id ou uma expressão.
--                       procname    params
startProcedure :: State -> String -> [(Type, Value)] -> State
startProcedure (State (SymbolTable structs procs funcs oldScope (Memory mem)) io) procName params =
    finalState
    where
        -- acha o proc
        (Procedure _ fields procTree) = findProc procs procName (fieldfy params)
        -- gera proximoescopo
        newStartingScopeDepth = findNextScopeDepthFromMem mem procName
        newStartingScope = Scope (procName, newStartingScopeDepth) (procName, 1)
        -- instancia coisas do proc
        newMem = instanciarParams mem params fields newStartingScope
        -- executa o proc no analisadorSemantico
        (State (SymbolTable _ _ _ _ (Memory procFinalMem)) finalIo) = analisadorSemantico procTree (State (SymbolTable structs procs funcs [newStartingScope] (Memory newMem)) io)
        -- deletar coisas do escopo aqui
        finalMem = cleanScopeFromMem procFinalMem newStartingScope
        -- temp só pra compilar
        finalState = (State (SymbolTable structs procs funcs oldScope (Memory finalMem)) finalIo)

--                        funcName    params
startFunction :: State -> TokenTree -> [(Type, Value)] -> (State, (Type, Value))
startFunction (State (SymbolTable structs procs funcs oldScope (Memory mem)) io) (LeafToken (Id _ funcName)) params =
    res
    where
        -- acha a func
        (Function _ retType fields funcTree) = findFunc funcs funcName (fieldfy params)
        -- gera proximoescopo
        newStartingScopeDepth = findNextScopeDepthFromMem mem funcName
        newStartingScope = Scope (funcName, newStartingScopeDepth) (funcName, 1)
        -- instancia coisas da func
        newMem = instanciarParams mem params fields newStartingScope
        -- executa a func no analisadorSemantico
        (State (SymbolTable _ _ _ _ (Memory funcFinalMem)) finalIo) = analisadorSemantico funcTree (State (SymbolTable structs procs funcs [newStartingScope] (Memory newMem)) io)
        -- Tentar pegar o retorno aqui
        returnedVal = getReturn funcFinalMem newStartingScope retType;
        -- deletar return aqui
        afterReturnDelMem = cleanScopeFromMem funcFinalMem (Scope (funcName, newStartingScopeDepth) ("return", 0))
        -- deletar coisas do escopo aqui
        finalMem = cleanScopeFromMem afterReturnDelMem newStartingScope
        -- valor de retorno
        res = ((State (SymbolTable structs procs funcs oldScope (Memory finalMem)) finalIo), returnedVal)

getReturn :: [Variable] -> Scope -> Type -> (Type, Value)
getReturn mem (Scope funcScope (_, _)) typ = 
    case res of
        Nothing -> error "Nenhum valor foi retornado na função"
        Just (Variable _ varTyp varVal _) ->
            if (varTyp == typ) then
                (typ, varVal)
            else 
                error "Tipo de retorno incorreto"
    where
        res = lookUpAux mem "return" (Scope funcScope ("return", 0))

-- Find next scope depth
findNextScopeDepthFromMem :: [Variable] -> String -> Int
findNextScopeDepthFromMem [] _ = 1
findNextScopeDepthFromMem ((Variable _ _ _ (Scope (scName, scDepth) (_,_))):mem) name =
    if scName == name then
        if (scDepth + 1) > res then
            scDepth + 1
        else
            res
    else
        res
    where 
        res = findNextScopeDepthFromMem mem name

cleanScopeFromMem :: [Variable] -> Scope -> [Variable]
cleanScopeFromMem [] _ = []
cleanScopeFromMem ((Variable id typ val valEsc):mem) esc =
    if valEsc == esc
        then cleanScopeFromMem mem esc
    else (Variable id typ val valEsc):(cleanScopeFromMem mem esc)

startBlock :: TokenTree -> State -> String -> State
startBlock stmts (State (SymbolTable a b c scopes mem) io) blocName = finalState
    where

        (Scope (func, funcDepth) (_, _)) = head scopes
        nextScope = (Scope (func, funcDepth) (blocName, findNextScopeDepthFromScope scopes blocName))
        (State (SymbolTable _ _ _ _ (Memory afterMem)) finalIO) = analisadorSemantico stmts (State (SymbolTable a b c (nextScope:scopes) mem) io)
        finalMem = cleanScopeFromMem afterMem nextScope
        finalState = (State (SymbolTable a b c scopes (Memory finalMem)) finalIO)

resolveWhile :: State -> TokenTree -> TokenTree -> State
resolveWhile st cond stmts = 
    if (solvedCond) then
        resolveWhile (startBlock stmts stFinal "while") cond stmts
    else
        stFinal
    where
        (stFinal, solvedCond) = checkCondition st cond

--               estadoInicial condição       bloco        resto      final
resolveIfCondition :: State -> TokenTree -> TokenTree -> TokenTree -> State
resolveIfCondition st cond statements None =
    if (solvedCond) then
        startBlock statements finalState "if"
    else
        finalState
    where
        (finalState, solvedCond) = checkCondition st cond
resolveIfCondition st cond statements (UniTree NonTElse elseStmts) =
    if (solvedCond) then
        startBlock statements finalState "if"
    else
        startBlock elseStmts finalState "if"
    where
        (finalState, solvedCond) = checkCondition st cond
resolveIfCondition st cond statements (TriTree NonTIf nextCond nextStmts nextBlock) =
    if (solvedCond) then
        startBlock statements finalState "if"
    else
        resolveIfCondition finalState nextCond nextStmts nextBlock
    where
        (finalState, solvedCond) = checkCondition st cond

checkCondition :: State -> TokenTree -> (State, Bool)
checkCondition st cond = case exprRes of
    (finalState, (BoolType, (Bool res))) -> (finalState, res)
    (st, a) -> error ("A expressão utilizada na condição deve ser do tipo bool " ++ (show a))
    where
        exprRes = avaliarExpressao st cond

findNextScopeDepthFromScope :: [Scope] -> String -> Int
findNextScopeDepthFromScope [] _ = 1
findNextScopeDepthFromScope ((Scope (scName, scDepth) (_,_)):scopes) name =
    if scName == name then
        if (scDepth + 1) > res then
            scDepth + 1
        else
            res
    else
        res
    where 
        res = findNextScopeDepthFromScope scopes name

-- Como funções podem ser chamadas de dentro de expressões e funções podem modificar o
-- estado, então o estado todo tem que ser passado pra o avaliador de expr
--                 estado    arvoreExpr   estadofinal e valor encontrado
avaliarExpressao :: State -> TokenTree -> (State, (Type, Value))
avaliarExpressao st tree = case tree of
    -- literais
    LeafToken a -> case a of
        IntLit _ v -> (st, (IntType, Int v))
        FloatLit _ v -> (st, (FloatType, Float v))
        StrLit _ v -> (st, (StringType, String v))
        SymBoolTrue _ -> (st, (BoolType, Bool True))
        SymBoolFalse _ -> (st, (BoolType, Bool False))
    UniTree nonT a -> case nonT of
        NonTInvokeFunction -> startFunction st a []
        NonTId -> avaliarExpressaoParseId st a
    DualTree nonT a b -> case nonT of
        NonTExpr -> case a of 
            -- ! a
            (LeafToken (SymBoolNot _)) -> res
            where
                (st1, (type1, val1)) = avaliarExpressao st a
                res = (st1, exprSum (type1, val1) (type1, val1)) -- mudar função
        
        NonTInvokeFunctionArgs -> error "não implementado ainda" -- res
            -- where
                -- (State, [(Type, Value)])
                -- (st1, (args)) = evalArgs st b
                -- nome = algo
                -- res = startProcedure st1 nome args -- modificar para funçao
    TriTree nonT a b c -> case nonT of
        NonTExpr -> triTreeExprParser st b a c

avaliarExpressaoParseId :: State -> TokenTree -> (State, (Type, Value))
avaliarExpressaoParseId (State (SymbolTable a b c scopes (Memory mem)) io) (LeafToken (Id _ id)) = res
    where
        (Variable _ typ val _) = lookUpScoped mem id scopes
        res = ((State (SymbolTable a b c scopes (Memory mem)) io), (typ, val))
avaliarExpressaoParseId st (UniTree NonTPtrOp a) = dereferencePtr (avaliarExpressaoParseId st a)
avaliarExpressaoParseId st (DualTree NonTArray idm indexes) =
    getMatrixVal st1 typ searchedVal indexes
    where
        (st1, (typ, searchedVal)) = avaliarExpressaoParseId st idm

dereferencePtr :: (State, (Type, Value)) -> (State, (Type, Value))
dereferencePtr ((State (SymbolTable a b c d (Memory mem)) io), (_, Pointer id esc)) = res
    where
        (Variable _ typ val _) = lookUpWrapper mem id esc
        res = ((State (SymbolTable a b c d (Memory mem)) io), (typ, val))


getMatrixVal :: State -> Type -> Value -> TokenTree -> (State, (Type, Value))
getMatrixVal st (ListType typ) var (DualTree NonTListIndex expr next) = res
    where
        (st2, (_, exprVal)) = avaliarExpressao st expr
        res = getMatrixVal st2 typ (accessListAt var exprVal) next

getMatrixVal st (ListType typ) var (UniTree NonTIndex expr) = res
    where
        (st2, (_, exprVal)) = avaliarExpressao st expr
        val = accessListAt var exprVal
        res = (st2, (typ, val))

--                      Estado   Operação     op1          op2           resultado
triTreeExprParser :: State -> TokenTree -> TokenTree -> TokenTree -> (State, (Type, Value))
-- a + b
triTreeExprParser st (LeafToken (SymOpPlus _)) a c = res
    where 
        (st1, (type1, val1)) = avaliarExpressao st a
        (st2, (type2, val2)) = avaliarExpressao st1 c
        res = (st2, exprSum (type1, val1) (type2, val2))

-- a - b
triTreeExprParser st (LeafToken (SymOpMinus _)) a c = res
    where
        (st1, (type1, val1)) = avaliarExpressao st a
        (st2, (type2, val2)) = avaliarExpressao st1 c
        res = (st2, exprMinus (type1, val1) (type2, val2))

-- a * b
triTreeExprParser st (LeafToken (SymOpMult _)) a c = res
    where
        (st1, (type1, val1)) = avaliarExpressao st a
        (st2, (type2, val2)) = avaliarExpressao st1 c
        res = (st2, exprMult (type1, val1) (type2, val2))

-- a / b
triTreeExprParser st (LeafToken (SymOpDiv _)) a c = res
    where
        (st1, (type1, val1)) = avaliarExpressao st a
        (st2, (type2, val2)) = avaliarExpressao st1 c
        res = (st2, exprDiv (type1, val1) (type2, val2))

-- a % b
triTreeExprParser st (LeafToken (SymOpMod _)) a c = res
    where
        (st1, (type1, val1)) = avaliarExpressao st a
        (st2, (type2, val2)) = avaliarExpressao st1 c
        res = (st2, exprMod (type1, val1) (type2, val2))

-- a ^ b
triTreeExprParser st (LeafToken (SymOpExp _)) a c = res
    where
        (st1, (type1, val1)) = avaliarExpressao st a
        (st2, (type2, val2)) = avaliarExpressao st1 c
        res = (st2, exprExp (type1, val1) (type2, val2))

-- a == b
triTreeExprParser st (LeafToken (SymBoolEq _)) a c = res
    where
        (st1, (type1, val1)) = avaliarExpressao st a
        (st2, (type2, val2)) = avaliarExpressao st1 c
        res = (st2, exprBoolEq (type1, val1) (type2, val2)) -- falta ponteiros

-- a != b
triTreeExprParser st (LeafToken (SymBoolNotEq _)) a c = res
    where
        (st1, (type1, val1)) = avaliarExpressao st a
        (st2, (type2, val2)) = avaliarExpressao st1 c
        res = (st2, exprBoolNotEq (type1, val1) (type2, val2)) -- falta ponteiros

-- a <= b
triTreeExprParser st (LeafToken (SymBoolLessThanEq _)) a c = res
    where
        (st1, (type1, val1)) = avaliarExpressao st a
        (st2, (type2, val2)) = avaliarExpressao st1 c
        res = (st2, exprBoolLessThanEq (type1, val1) (type2, val2))

-- a < b
triTreeExprParser st (LeafToken (SymBoolLessThan _)) a c = res
    where
        (st1, (type1, val1)) = avaliarExpressao st a
        (st2, (type2, val2)) = avaliarExpressao st1 c
        res = (st2, exprBoolLessThan (type1, val1) (type2, val2)) 

-- a >= b
triTreeExprParser st (LeafToken (SymBoolGreaterThanEq _)) a c = res
    where
        (st1, (type1, val1)) = avaliarExpressao st a
        (st2, (type2, val2)) = avaliarExpressao st1 c
        res = (st2, exprBoolGreaterThanEq (type1, val1) (type2, val2))

-- a > b
triTreeExprParser st (LeafToken (SymBoolGreaterThan _)) a c = res
    where
        (st1, (type1, val1)) = avaliarExpressao st a
        (st2, (type2, val2)) = avaliarExpressao st1 c
        res = (st2, exprBoolGreaterThan (type1, val1) (type2, val2))

-- a && b
triTreeExprParser st (LeafToken (SymBoolAnd _)) a c = res
    where
        (st1, (type1, val1)) = avaliarExpressao st a
        (st2, (type2, val2)) = avaliarExpressao st1 c
        res = (st2, exprBoolAnd (type1, val1) (type2, val2))

-- a || b
triTreeExprParser st (LeafToken (SymBoolOr _)) a c = res
    where
        (st1, (type1, val1)) = avaliarExpressao st a
        (st2, (type2, val2)) = avaliarExpressao st1 c
        res = (st2, exprBoolOr (type1, val1) (type2, val2))

exprLessThan :: (Type, Value) -> (Type, Value) -> (Type, Value)
exprLessThan (IntType, Int a) (IntType, Int b) = (BoolType, Bool (a < b))
exprLessThan (FloatType, Float a) (FloatType, Float b) = (BoolType, Bool (a < b))

intToFloat :: Int -> Float
intToFloat a = fromInteger (toInteger a) 

exprSum :: (Type, Value) -> (Type, Value) -> (Type, Value)
exprSum (IntType, Int a) (IntType, Int b) = (IntType, Int (a + b))
exprSum (FloatType, Float a) (FloatType, Float b) = (FloatType, Float (a + b))
exprSum (IntType, Int a) (FloatType, Float b) = (FloatType, Float ( intToFloat a + b ))
exprSum (FloatType, Float a) (IntType, Int b) = (FloatType, Float ( a + intToFloat b ))
exprSum (StringType, String a) (StringType, String b) = (StringType, String (a ++ b))
exprSum (ListType t1, List a) (ListType t2, List b) = 
    if t1 == t2 
        then (ListType t1, List (a ++ b))
    else error "Concatenação entre listas de tipos diferentes não é permitida"
exprSum a b = error ("Operação entre os tipos " ++ (show a) ++ " e " ++ (show b) ++ " não é permitida")

exprMinus :: (Type, Value) -> (Type, Value) -> (Type, Value)
exprMinus (IntType, Int a) (IntType, Int b) = (IntType, Int (a - b))
exprMinus (FloatType, Float a) (FloatType, Float b) = (FloatType, Float (a - b))
exprMinus (IntType, Int a) (FloatType, Float b) = (FloatType, Float ( intToFloat a - b ))
exprMinus (FloatType, Float a) (IntType, Int b) = (FloatType, Float ( a - intToFloat b ))
exprMinus a b = error ("Operação entre os tipos " ++ (show a) ++ " e " ++ (show b) ++ " não é permitida")

exprMult :: (Type, Value) -> (Type, Value) -> (Type, Value)
exprMult (IntType, Int a) (IntType, Int b) = (IntType, Int (a * b))
exprMult (FloatType, Float a) (FloatType, Float b) = (FloatType, Float (a * b))
exprMult (IntType, Int a) (FloatType, Float b) = (FloatType, Float ( intToFloat a * b ))
exprMult (FloatType, Float a) (IntType, Int b) = (FloatType, Float ( a * intToFloat b ))
exprMult a b = error ("Operação entre os tipos " ++ (show a) ++ " e " ++ (show b) ++ " não é permitida")

exprDiv :: (Type, Value) -> (Type, Value) -> (Type, Value)
exprDiv (IntType, Int a) (IntType, Int b) = (IntType, Int (a `div` b))
exprDiv (FloatType, Float a) (FloatType, Float b) = (FloatType, Float (a / b))
exprDiv (IntType, Int a) (FloatType, Float b) = (FloatType, Float ( intToFloat a / b ))
exprDiv (FloatType, Float a) (IntType, Int b) = (FloatType, Float ( a / intToFloat b ))
exprDiv a b = error ("Operação entre os tipos " ++ (show a) ++ " e " ++ (show b) ++ " não é permitida")

exprMod :: (Type, Value) -> (Type, Value) -> (Type, Value)
exprMod (IntType, Int a) (IntType, Int b) = (IntType, Int (a `rem` b))
exprMod a b = error ("Operação entre os tipos " ++ (show a) ++ " e " ++ (show b) ++ " não é permitida")

exprExp :: (Type, Value) -> (Type, Value) -> (Type, Value)
exprExp (IntType, Int a) (IntType, Int b) = (IntType, Int (a ^ b))
exprExp (FloatType, Float a) (FloatType, Float b) = (FloatType, Float (a ** b))
exprExp (IntType, Int a) (FloatType, Float b) = (FloatType, Float ( intToFloat a ** b ))
exprExp (FloatType, Float a) (IntType, Int b) = (FloatType, Float ( a ** intToFloat b ))
exprExp a b = error ("Operação entre os tipos " ++ (show a) ++ " e " ++ (show b) ++ " não é permitida")

exprBoolEq :: (Type, Value) -> (Type, Value) -> (Type, Value)
exprBoolEq (IntType, Int a) (IntType, Int b) = (BoolType, Bool (a == b))
exprBoolEq (FloatType, Float a) (FloatType, Float b) = (BoolType, Bool (a == b))
exprBoolEq (IntType, Int a) (FloatType, Float b) = (BoolType, Bool ( intToFloat a == b ))
exprBoolEq (FloatType, Float a) (IntType, Int b) = (BoolType, Bool ( a == intToFloat b ))
exprBoolEq (StringType, String a) (StringType, String b) = (BoolType, Bool (a == b))
exprBoolEq (BoolType, Bool a) (BoolType, Bool b) = (BoolType, Bool (a == b))
exprBoolEq (PointerType tipoA, Pointer strA scpA) (PointerType tipoB, Pointer strB scpB) = 
    (BoolType, Bool (tipoA == tipoB && strA == strB && scpA == scpB))
exprBoolEq (ListType t1, List a) (ListType t2, List b) = (BoolType, Bool (a == b))
exprBoolEq a b = error ("Operação entre os tipos " ++ (show a) ++ " e " ++ (show b) ++ " não é permitida")

exprBoolNotEq :: (Type, Value) -> (Type, Value) -> (Type, Value)
exprBoolNotEq (IntType, Int a) (IntType, Int b) = (BoolType, Bool (a /= b))
exprBoolNotEq (FloatType, Float a) (FloatType, Float b) = (BoolType, Bool (a /= b))
exprBoolNotEq (IntType, Int a) (FloatType, Float b) = (BoolType, Bool ( intToFloat a /= b ))
exprBoolNotEq (FloatType, Float a) (IntType, Int b) = (BoolType, Bool ( a /= intToFloat b ))
exprBoolNotEq (StringType, String a) (StringType, String b) = (BoolType, Bool (a /= b))
exprBoolNotEq (BoolType, Bool a) (BoolType, Bool b) = (BoolType, Bool (a /= b))
exprBoolNotEq (PointerType tipoA, Pointer strA scpA) (PointerType tipoB, Pointer strB scpB) = 
    (BoolType, Bool (tipoA /= tipoB || strA /= strB || scpA /= scpB))
exprBoolNotEq (ListType t1, List a) (ListType t2, List b) = (BoolType, Bool (a /= b))
exprBoolNotEq a b = error ("Operação entre os tipos " ++ (show a) ++ " e " ++ (show b) ++ " não é permitida")

exprBoolLessThanEq :: (Type, Value) -> (Type, Value) -> (Type, Value)
exprBoolLessThanEq (IntType, Int a) (IntType, Int b) = (BoolType, Bool (a <= b))
exprBoolLessThanEq (FloatType, Float a) (FloatType, Float b) = (BoolType, Bool (a <= b))
exprBoolLessThanEq a b = error ("Operação entre os tipos " ++ (show a) ++ " e " ++ (show b) ++ " não é permitida")

exprBoolLessThan :: (Type, Value) -> (Type, Value) -> (Type, Value)
exprBoolLessThan (IntType, Int a) (IntType, Int b) = (BoolType, Bool (a < b))
exprBoolLessThan (FloatType, Float a) (FloatType, Float b) = (BoolType, Bool (a < b))
exprBoolLessThan a b = error ("Operação entre os tipos " ++ (show a) ++ " e " ++ (show b) ++ " não é permitida")

exprBoolGreaterThanEq :: (Type, Value) -> (Type, Value) -> (Type, Value)
exprBoolGreaterThanEq (IntType, Int a) (IntType, Int b) = (BoolType, Bool (a >= b))
exprBoolGreaterThanEq (FloatType, Float a) (FloatType, Float b) = (BoolType, Bool (a >= b))
exprBoolGreaterThanEq a b = error ("Operação entre os tipos " ++ (show a) ++ " e " ++ (show b) ++ " não é permitida")

exprBoolGreaterThan :: (Type, Value) -> (Type, Value) -> (Type, Value)
exprBoolGreaterThan (IntType, Int a) (IntType, Int b) = (BoolType, Bool (a > b))
exprBoolGreaterThan (FloatType, Float a) (FloatType, Float b) = (BoolType, Bool (a > b))
exprBoolGreaterThan a b = error ("Operação entre os tipos " ++ (show a) ++ " e " ++ (show b) ++ " não é permitida")

exprBoolAnd :: (Type, Value) -> (Type, Value) -> (Type, Value)
exprBoolAnd (BoolType, Bool a) (BoolType, Bool b) = (BoolType, Bool (a && b))
exprBoolAnd a b = error ("Operação entre os tipos " ++ (show a) ++ " e " ++ (show b) ++ " não é permitida")

exprBoolOr :: (Type, Value) -> (Type, Value) -> (Type, Value)
exprBoolOr (BoolType, Bool a) (BoolType, Bool b) = (BoolType, Bool (a || b))
exprBoolOr a b = error ("Operação entre os tipos " ++ (show a) ++ " e " ++ (show b) ++ " não é permitida")

--                   memoria       params             campos    escopo    memoria atualizada
instanciarParams :: [Variable] -> [(Type, Value)] -> [Field] -> Scope -> [Variable]
instanciarParams mem ((typeValue, value):params) ((Field typeField id):fields) escopo = finalMem
    where
        nextMem = ((Variable id typeField value escopo):mem)
        finalMem = (instanciarParams nextMem params fields escopo)
instanciarParams mem [] [] _ = mem
instanciarParams _ _ _ _ = error "Erro inesperado em instanciarParams"

--             params         
fieldfy :: [(Type, Value)] -> [Field]
fieldfy [] = []
fieldfy ((t, v):params) = f:(fieldfy params)
    where
        f = Field t []

--         procedimentos    nome      tipos
findProc :: [Procedure] -> String -> [Field] -> Procedure
findProc [] _ _ = error "Procedimento não declarado"
findProc ((Procedure procName procFields tree):procs) tarName tarFields =
    if procName == tarName && procFields == tarFields
        then (Procedure procName procFields tree)
    else findProc procs tarName tarFields

--            funcoes      nome      tipos
findFunc :: [Function] -> String -> [Field] -> Function
findFunc [] _ _ = error "Função não declarado"
findFunc ((Function funcName retType funcFields tree):funcs) tarName tarFields =
    if funcName == tarName && funcFields == tarFields
        then (Function funcName retType funcFields tree)
    else findFunc funcs tarName tarFields

-- TODO: NÃO TESTADA
-- Se os tipos forem incorretos joga erro
--              Lista    Index
accessListAt :: Value -> Value -> Value
accessListAt (List a) (Int i) = returnNthOfList a i
accessListAt (List a) _ = error "indice inválido, esperava inteiro"
accessListAt _ _ = error "Valor passado não é uma lista"

returnNthOfList :: [a] -> Int -> a
returnNthOfList [] _ = error "Out of range"
returnNthOfList (val:l) 0 = val
returnNthOfList (val:l) n = returnNthOfList l (n-1)

-- TODO: NÃO TESTADA 
-- Se não achar instancia, se achar joga erro
--                memoria  id/tipo/escopo/valor
instanciarVar :: [Variable] -> Variable -> [Variable]
instanciarVar mem (Variable id typ val sc) =
    if (lookUpAux mem id sc) == Nothing
        then (Variable id typ val sc):mem
    else error "Variável tentando ser redeclarada"

-- TODO: NÃO TESTADA 
--           memoria  id/tipo/escopo/valor
atribuirVar :: [Variable] -> Variable -> [Variable]
atribuirVar [] _ = error "Variável não declarada"
atribuirVar ((Variable vId vTyp vVal vSc):mem) (Variable id typ val sc) =
    if vId == id && vSc == sc
        then if vTyp == typ
            then ((Variable vId vTyp val vSc):mem)
        else 
            error "Tipo incorreto"
    else 
        (Variable vId vTyp vVal vSc):(atribuirVar mem (Variable id typ val sc))

-- TODO: Testar toda a família de funções criarValorParaAtribuicao
-- Essa função faz um abuso do tipo Variable, reutilizando seus campos pra armazenar
-- os valores de uma forma diferente da semântica original atribuida a ela.
-- Em [parte1] o tipo é na verdade o tipo esperado do variável, enquanto valor é o
-- valor final que será atribuído a ela e id/escopo se referem ao id/escopo da
-- variável alvo real.

-- Essa função será chamada pela parte principal do programa com o valor final determinado
-- pela resolução de uma expressão. Aqui a variável tentará ser criada para ser posta na
-- memória e após isso basta chamar a função de atribuição nela
--                          estado    idMagico    doquevaiseratrib escopoatual
criarValorParaAtribuicao :: State -> TokenTree -> (Type, Value) -> (State, Variable)
criarValorParaAtribuicao (State (SymbolTable strts procs funcs esc (Memory mem)) io) (UniTree NonTId id) (newTyp, newVal) =
    if newTyp == (criarValorParaAtribuicao_unfoldTypes realTyp list)
        then (finalSt, (criarValorFinal mem ((Variable targetId realTyp finalVal targetEsc), list)))
    else error "Tipos incompatíves na atribuição"
    where
        (finalSt, ((Variable targetId realTyp finalVal targetEsc), list)) = encontrarVarAlvo (State (SymbolTable strts procs funcs esc (Memory mem)) io) id newVal

criarValorParaAtribuicao_unfoldTypes :: Type -> [Value] -> Type
criarValorParaAtribuicao_unfoldTypes typ [] = typ
criarValorParaAtribuicao_unfoldTypes (ListType typ) (a:inds) = criarValorParaAtribuicao_unfoldTypes typ inds

-- IdMagico = IdNormal String | DereferenceOf IdMagico | AtIndex IdMagico Value
-- Special = Spec Variable [Int]
--              memoria      idMagico  aseratrib escopoatual     (Variável carrega o tipo ESPERADO)
--                                                               (Também o valor final)
encontrarVarAlvo :: State -> TokenTree -> Value -> (State, (Variable, [Value]))
-- IdBase
encontrarVarAlvo st (LeafToken (Id _ id)) newVal = var
    where
        (State (SymbolTable _ _ _ esc (Memory mem)) _) = st
        (Variable _ typ oldval varEsc) = lookUpScoped mem id esc
        var = (st, ((Variable id typ newVal varEsc), []))
-- Pointers
encontrarVarAlvo st (UniTree NonTPtrOp idm) val =
    if checkForPtrType solvedValue
        then (finalSt, (encontrarVarAlvo_decypherPtr mem (Variable newId typ solvedValue newEsc), []))
    else error "Tentando dereferenciar algo que não é ponteiro"
    where
        (finalSt, ((Variable newId typ newVal newEsc), newList)) = encontrarVarAlvo st idm val
        (State (SymbolTable _ _ _ _ (Memory mem)) _) = finalSt
        solvedValue = (encontrarVarAlvo_getMatrixVal (newVal, newList))
-- Array
encontrarVarAlvo st (DualTree NonTArray idm indexes) val =
    (finalSt, (var, (prevIndexes ++ nextIndexes)))
    where
        (st1, (var, prevIndexes)) = encontrarVarAlvo st idm val
        (finalSt, nextIndexes) = encontrarVarAlvo_translateIndexes st1 indexes

encontrarVarAlvo_translateIndexes :: State -> TokenTree -> (State, [Value])
encontrarVarAlvo_translateIndexes st (DualTree NonTListIndex expr next) = res
    where
        (st2, (_, exprVal)) = avaliarExpressao st expr
        (finalSt, vals) = encontrarVarAlvo_translateIndexes st2 next
        res = (finalSt, (exprVal:vals))

encontrarVarAlvo_translateIndexes st (UniTree NonTIndex expr) = res
    where
        (st2, (_, exprVal)) = avaliarExpressao st expr
        res = (st2, [exprVal])

encontrarVarAlvo_listAdd :: Variable -> [Value] -> Value -> (Variable, [Value])
encontrarVarAlvo_listAdd (Variable oldId (ListType typ) oldval oldEsc) list newIndex =
    ((Variable oldId typ oldval oldEsc), (list ++ [newIndex]))
encontrarVarAlvo_listAdd _ _ _ = error "Tentando acessar o índice de algo que não é uma lista"

checkForPtrType :: Value -> Bool
checkForPtrType (Pointer _ _) = True
checkForPtrType _ = False

encontrarVarAlvo_decypherPtr :: [Variable] -> Variable -> Variable
encontrarVarAlvo_decypherPtr [] _ = error "Variável não encontrada: Ponteiro não aponta pra nenhuma variável em memória"
encontrarVarAlvo_decypherPtr ((Variable vId vTyp (Pointer realId realEsc) vEsc):mem) (Variable id (PointerType typ) val esc) =
    if vId == id && vEsc == esc
        then (Variable realId typ val realEsc)
    else
        encontrarVarAlvo_decypherPtr mem (Variable id typ val esc)
encontrarVarAlvo_decypherPtr (var:mem) ptr = encontrarVarAlvo_decypherPtr mem ptr

--                   Valor(m ou não) indices valor final
encontrarVarAlvo_getMatrixVal :: (Value, [Value]) -> Value
encontrarVarAlvo_getMatrixVal (val, []) = val
encontrarVarAlvo_getMatrixVal ((List val), ((Int i):l)) = encontrarVarAlvo_getMatrixVal ((returnNthOfList val i), l)
encontrarVarAlvo_getMatrixVal (val, (_:l)) = error "Index inválido"

--               memoria       Special          Variavel final
criarValorFinal :: [Variable] -> (Variable, [Value]) -> Variable
criarValorFinal mem ((Variable idm typ val escm), indexes) =
    Variable idm typ finalVal escm
    where
        Variable currId currTyp currVal currEsc = lookUpWrapper mem idm escm
        finalVal = criarValorFinal_resolveMatrix currVal val indexes

--            ValorDaV  NovoValor Indices   ValorFinal
criarValorFinal_resolveMatrix :: Value -> Value -> [Value] -> Value
criarValorFinal_resolveMatrix oldVal newVal [] = newVal
criarValorFinal_resolveMatrix (List vals) newVal ((Int i):l) = 
    (List ((listUpTo vals i) ++ [modVal] ++ (listFromToEnd vals (i+1))))
    where
        modVal = (criarValorFinal_resolveMatrix (returnNthOfList vals i) newVal l)

listUpTo :: [a] -> Int -> [a]
listUpTo _ 0 = []
listUpTo (h:l) i = h:(listUpTo l (i-1))
listUpTo [] _ = error "Out of bounds"

listFromToEnd :: [a] -> Int -> [a]
listFromToEnd l 0 = 
    case l of
        [] -> error "Out of bounds"
        l -> l
listFromToEnd (h:l) i = listFromToEnd l (i-1)
listFromToEnd _ _ = error "Out of bounds" -- implica que a lista é vazia e indice > 0

-- TODO: NÃO TESTADA
printAll :: State -> TokenTree -> State
printAll st (DualTree NonTParams expr next) = printAll st1 next
    where
        ((State midTable midIO), (typ, val)) = avaliarExpressao st expr
        nextIO = midIO >> (printOne val)
        st1 = (State midTable nextIO)
printAll st (UniTree NonTParam expr) = (State midTable finalIO)
    where
        ((State midTable midIO), (typ, val)) = avaliarExpressao st expr
        finalIO = midIO >> (printOne val) >> (putStrLn "")

printOne :: Value -> IO()
printOne val = case val of
    (Int a) -> putStr ((show a)++" ") 
    (Float a) -> putStr ((show a)++" ") 
    (String a) -> putStr a
    (Bool a) -> putStr ((show a)++" ") 
    (Pointer _ _) -> putStr "-Ponteiro- "
    (List a) -> (putStr "[ ") >> (printList a) >> (putStr "]")
    _ -> error "Impossível imprimir structs"

printList :: [Value] -> IO()
printList [] = putStr ""
printList (a:vals) = (printOne a) >> (printList vals)

-- TODO: NÃO TESTADA
-- Deprecated
--        memoria     id      escopo
lookUp :: Memory -> String -> Scope -> Variable
lookUp (Memory mem) id escopo =
    case lookUpAux mem id escopo of
        Nothing -> error "Variável não declarada"
        Just res -> res

--                memoria        id      escopo
lookUpWrapper :: [Variable] -> String -> Scope -> Variable
lookUpWrapper a b c = 
    case lookUpAux a b c of
        Nothing -> error "Variável não declarada"
        Just res -> res

--               memoria        id       escopo
lookUpScoped :: [Variable] -> String -> [Scope] -> Variable
lookUpScoped mem id [] = error "Variável não declarada"
lookUpScoped mem id (currEsc:esc) = 
    case lookUpAux mem id currEsc of
        Nothing -> lookUpScoped mem id esc
        Just res -> res

-- TODO: NÃO TESTADA
lookUpAux :: [Variable] -> String -> Scope -> Maybe Variable
lookUpAux [] _ _ = Nothing
lookUpAux ((Variable id ty val sc):vars) id2 sc2 = 
    if id2 == id && sc == sc2
        then Just (Variable id ty val sc)
    else lookUpAux vars id2 sc2

-- Main

main :: IO ()
main = case unsafePerformIO (parser (getTokens "arquivo.in")) of
            { Left err -> print err; 
              Right ans -> inicAnalisadorSemantico ans
            }