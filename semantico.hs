import Sintatico
import Lexico

import System.IO.Unsafe
import System.Environment


-- ListaDeStructs(nome [campos (nome typo)]) ListaDeProcedimentos(Id Parâmetros BlocoDaFunção) ListaDeFunções(Id TipodeRetorno Parâmetros BlocoDaFunção) ListadeEscopos ListaDeVariáveis(Id Tipo Valor Escopo)
data State = State SymbolTable (IO ()) FlowFlag
data FlowFlag = NoneFlag | BreakFlag | ContinueFlag | ReturnFlag deriving (Eq, Show)
--                             structs  procedimentos funções escAtuais memoria
data SymbolTable = SymbolTable [Struct] [Procedure] [Function] [Scope] Memory deriving (Eq, Show)
--                          nome   campos   bloco
data Procedure = Procedure String [Field] TokenTree |
--                             nome  campos      procedimento em haskell
                 SysProcedure String [Field] (State -> [(Type, Value)] -> State)
instance Eq Procedure where
    (SysProcedure a1 b1 c1) == (SysProcedure a2 b2 c2) = ((a1 == a2) && (b1 == b2))
    (Procedure a1 b1 c1) == (Procedure a2 b2 c2) = ((a1 == a2) && (b1 == b2) && (c1 == c2))
instance Show Procedure where
    show (SysProcedure a b c) = show a ++ " " ++ show b  ++ " sys procedure"
    show (Procedure a b c) = show a ++ " " ++ show b  ++ " " ++ show c

--                        nome retorno campos bloco
data Function = Function String Type [Field] TokenTree |
                SysFunction String Type [Field] (State -> [(Type, Value)] -> (State, (Type, Value)))
instance Eq Function where
    (SysFunction a1 b1 c1 d1) == (SysFunction a2 b2 c2 d2) = ((a1 == a2) && (b1 == b2) && (c1 == c2))
    (Function a1 b1 c1 d1) == (Function a2 b2 c2 d2) = ((a1 == a2) && (b1 == b2) && (c1 == c2) && (d1 == d2))
instance Show Function where
    show (SysFunction a b c d) = show a ++ " " ++ show b  ++ " " ++ show c ++ " sys function"
    show (Function a b c d) = show a ++ " " ++ show b  ++ " " ++ show c ++ " " ++ show d
                
--                    nome  campos
data Struct = Struct String [Field] deriving (Eq, Show)
--                 (nome e profundidade do subprograma) (nome e profundidade do subbloco)
data Scope = Scope (String, Int) (String, Int) deriving (Eq, Show)
--           campo tipo  nome
data Field = Field Type String deriving (Show)
-- Basta que os tipos sejam iguais para um field ser igual a outro.
instance Eq Field where
    (Field type1 name1) == (Field type2 name2) = (type1 == type2)
--                 listaDeVariaveis
data Memory = Memory [Variable] deriving (Eq, Show)
--                         id   tipo valor escopo
data Variable = Variable String Type Value Scope deriving (Eq, Show)

data Type = IntType | FloatType | StringType | BoolType | ListType Type | PointerType Type | StructType String | AnyType deriving (Show)
instance Eq Type where
    (IntType) == (IntType) = True
    (FloatType) == (FloatType) = True
    (StringType) == (StringType) = True
    (BoolType) == (BoolType) = True
    (ListType a) == (ListType b) = (a == b)
    (PointerType a) == (PointerType b) = (a == b)
    (StructType a) == (StructType b) = (a == b)
    (AnyType) == (_) = True
    (_) == (AnyType) = True
    (_) == (_) = False

data Value = Int Int |
             Float Float |
             String String |
             Bool Bool |
             List [Value] |
--                     id   escopo
             Pointer String Scope |
             StructVal [FieldInstance] deriving (Eq, Show)
data FieldInstance = FieldInstance Field Value deriving (Eq, Show)

-- FUNÇÕES E PROCEDIMENTOS DO SISTEMA
setListSize :: State -> [(Type, Value)] -> State
setListSize st ((_, (Pointer id scp)):(_, (Int size)):[]) = finalState
    where
        (State (SymbolTable a b c d (Memory mem)) io flag) = st
        (Variable _ (ListType subtyp) (List val) _) = lookUpWrapper mem id scp
        finalVal = setListSizeAux st val subtyp size
        finalMem = atribuirVar mem (Variable id (ListType subtyp) (List finalVal) scp)
        finalState = (State (SymbolTable a b c d (Memory finalMem)) io flag)

setListSizeAux :: State -> [Value] -> Type -> Int -> [Value]
setListSizeAux st l _ 0 = []
setListSizeAux st [] t i = (defaultVal st t):(setListSizeAux st [] t (i-1))
setListSizeAux st (e:l) t i = e:(setListSizeAux st l t (i-1))

pushBack :: State -> [(Type, Value)] -> State
pushBack st (((PointerType (ListType typ1)), (Pointer id scp)):(typ2, newVal):[]) =
    if (typ1 == typ2) then
        finalState
    else error ("Tipos incompatíveis na função pushBack: esperava " ++ (show typ1) ++ " e encontrou-se "++ (show typ2))
    where
        (State (SymbolTable a b c d (Memory mem)) io flag) = st
        (Variable _ _ (List val) _) = lookUpWrapper mem id scp
        finalVal = val ++ [newVal]
        finalMem = atribuirVar mem (Variable id (ListType typ1) (List finalVal) scp)
        finalState = (State (SymbolTable a b c d (Memory finalMem)) io flag)

listLength :: State -> [(Type, Value)] -> (State, (Type, Value))
listLength st ((ListType _, List val):[]) = (st, (IntType, Int (length val)))
listLength st ((StringType, (String val)):[]) = (st, (IntType, Int (length val)))

-- FUNÇÕES DO ANALISADOR SINTÁTICO
emptyState :: State
emptyState = State (SymbolTable [] startingProcedures startingFunctions [(Scope ("main", 1) ("main", 1))] (Memory [])) (return ()) NoneFlag

-- coloque as funções feitas aqui
startingProcedures :: [Procedure]
startingProcedures = [(SysProcedure "setListSize" [(Field (PointerType (ListType AnyType)) ""),(Field IntType "")] setListSize),
                      (SysProcedure "pushBack" [(Field (PointerType (ListType AnyType)) ""),(Field AnyType "")] pushBack)] 
startingFunctions :: [Function]
startingFunctions = [(SysFunction "len" IntType [(Field (ListType AnyType) "")] listLength),
                     (SysFunction "len" IntType [(Field StringType "")] listLength)]

inicAnalisadorSemantico :: TokenTree -> IO()
inicAnalisadorSemantico tree = getAnalisadorIO (analisadorSemantico tree emptyState)

getAnalisadorIO :: State -> IO()
getAnalisadorIO (State _ io _) = io

analisadorSemantico :: TokenTree -> State -> State
-- printando a tabela ao fim da execucao
-- analisadorSemantico (UniTree NonTProgram a) (State table io flag) = 
--     State table2 ((print table2) >> io2) fflag
--     where
--         (State table2 io2 fflag) = analisadorSemantico a (State table io flag)

-- analisadorSemantico (DualTree NonTProgram a b) st =
--     State table ((print table) >> io) fflag
--     where
--         (State table io fflag) = analisadorSemantico b st1
--         st1 = analisadorSemantico a st

-- analisadorSemantico (TriTree NonTProgram a b c) st =
--     State table ((print table) >> io) fflag
--     where
--         (State table io fflag) = analisadorSemantico c st2
--         st2 = analisadorSemantico b st1
--         st1 = analisadorSemantico a st

-- debug
analisadorSemantico (LeafToken (DebugTok _)) (State table io flag) = (State table ((print table) >> io) flag)

-- decl
analisadorSemantico (DualTree NonTDecl declType ids) st = declareMany st (parseType declType) ids
-- assign
analisadorSemantico (DualTree NonTAssign a c) st = assignToId st a c
-- print
analisadorSemantico (UniTree NonTPrint params) st = printAll st params
-- point to
analisadorSemantico (DualTree NonTPointTo a b) st = pointToId st a b
-- delete
analisadorSemantico (UniTree NonTDelPtr id) st = deletePointer st id
-- for
-- analisadorSemantico (TriTree NonTFor a b c) (State table io) = error "não implementado"
-- while
analisadorSemantico (DualTree NonTWhile a b) st = resolveWhile st a b
-- if 
analisadorSemantico (TriTree NonTIf a b c) st = resolveIfCondition st a b c
-- procCall
analisadorSemantico (DualTree NonTCallProcedure (LeafToken (Id _ id)) b) st = startProcedure st1 id args
    where
        (st1, args) = evalArgs st b
-- break
-- analisadorSemantico (LeafToken Break) (State table io) = error "não implementado"
-- return
analisadorSemantico (UniTree NonTReturn a) st = setReturnVal st a
-- continue
-- analisadorSemantico (LeafToken Continue) (State table io) = error "não implementado"

-- declStruc
analisadorSemantico (DualTree NonTStructDecl id decls) st =
    declareStruct st id decls
-- declFunc
analisadorSemantico (QuadTree NonTFuncDecl id params retType stmts) st =
    declareFunction st id params retType stmts
-- declProc
analisadorSemantico (TriTree NonTProcDecl id params stmts) st =
    declareProcedure st id params stmts

-- statements
-- para de executar quando alguma flag está ativa
analisadorSemantico (DualTree NonTStatements a b) st =
    case flag of
        ReturnFlag -> st1
        _ -> analisadorSemantico b st1
    where
       st1 = analisadorSemantico a st
       (State _ _ flag) = st1

analisadorSemantico (UniTree NonTStatement a) st = 
    analisadorSemantico a st

-- -- repetidores genericos
-- nontfuncdecls nontprocdecls e nontstructdecls caem por aqui
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
        (State (SymbolTable a b c (currScope:scopes) (Memory mem)) io flag) = st
        newVar = Variable id typ (defaultVal st typ) currScope
        finalMem = instanciarVar mem newVar
        finalSt = (State (SymbolTable a b c (currScope:scopes) (Memory finalMem)) io flag)
declareMany st typ (DualTree NonTListIds (DualTree NonTAssign (UniTree NonTId (LeafToken (Id _ id))) expr) rest) =
    declareMany finalSt typ rest
    where
        (st1, (exprTyp, exprVal)) = avaliarExpressao st expr
        (State (SymbolTable a b c (currScope:scopes) (Memory mem)) io flag) = st1
        finalMem = declareWithVal mem id typ exprTyp exprVal currScope
        finalSt = (State (SymbolTable a b c (currScope:scopes) (Memory finalMem)) io flag)
declareMany st typ (LeafToken (Id _ id)) = finalSt
    where
        (State (SymbolTable a b c (currScope:scopes) (Memory mem)) io flag) = st
        newVar = Variable id typ (defaultVal st typ) currScope
        finalMem = instanciarVar mem newVar
        finalSt = (State (SymbolTable a b c (currScope:scopes) (Memory finalMem)) io flag)
declareMany st typ (DualTree NonTAssign (UniTree NonTId (LeafToken (Id _ id))) expr) = finalSt
    where
        (st1, (exprTyp, exprVal)) = avaliarExpressao st expr
        (State (SymbolTable a b c (currScope:scopes) (Memory mem)) io flag) = st1
        finalMem = declareWithVal mem id typ exprTyp exprVal currScope
        finalSt = (State (SymbolTable a b c (currScope:scopes) (Memory finalMem)) io flag)
        

declareWithVal :: [Variable] -> String -> Type -> Type -> Value -> Scope -> [Variable]
declareWithVal mem id expectedType realType val currScope =
    if expectedType == realType then
        instanciarVar mem (Variable id realType val currScope)
    else error "Erro na declaração: Tipo da expressão não corresponde ao tipo declarado"

defaultVal :: State -> Type -> Value
defaultVal _ IntType = Int 0
defaultVal _ FloatType = Float 0.0
defaultVal _ StringType = String []
defaultVal _ BoolType = Bool False
defaultVal _ (PointerType _) = Pointer [] (Scope ([],0) ([],0))
defaultVal _ (ListType _) = List []
defaultVal st (StructType id) = StructVal (defaultStructVal st fields)
    where
        (State (SymbolTable structs b c d e) _ _) = st
        (Struct _ fields) = (findStructWrapper structs id)

defaultStructVal :: State -> [Field] -> [FieldInstance]
defaultStructVal _ [] = []
defaultStructVal st ((Field typ id):fields) = (FieldInstance (Field typ id) (defaultVal st typ)):(defaultStructVal st fields) 

parseType :: TokenTree -> Type
parseType (UniTree NonTListType a) = ListType (parseType a)
parseType (UniTree NonTPtrType a) = PointerType (parseType a)
parseType (UniTree NonTStructType (LeafToken (Id _ id))) = StructType id
parseType (LeafToken (TypeInt _)) = IntType
parseType (LeafToken (TypeFloat _)) = FloatType
parseType (LeafToken (TypeString _)) = StringType
parseType (LeafToken (TypeBoolean _)) = BoolType
parseType _ = error "Erro inesperado em parseType"

getInput :: String
getInput = unsafePerformIO (getLine)

getInputOfType :: Token -> (Type, Value)
getInputOfType (TypeInt _) = (IntType, Int value)
    where
        value = read getInput :: Int
getInputOfType (TypeFloat _) = (FloatType, Float value)
    where
        value = read getInput :: Float
getInputOfType (TypeString _) = (StringType, String value)
    where
        value = getInput :: String
getInputOfType _ = error "Operação de scan não permitida para esse tipo"

stob :: String -> Bool
stob str =
    if str == "true" then
        True
    else if str == "false" then
        False
    else error "Falha na conversão de string para bool"

assignToId :: State -> TokenTree -> TokenTree -> State
assignToId st id (UniTree NonTScan (LeafToken typ)) = finalState
    where
        valor = getInputOfType typ
        (st1, exprRes) = (st, valor)
        (st2, finalVal) = criarValorParaAtribuicao st1 id exprRes
        (State (SymbolTable a b c d (Memory mem)) io flag) = st2
        finalMem = atribuirVar mem finalVal
        finalState = (State (SymbolTable a b c d (Memory finalMem)) io flag)
assignToId st id expr = finalState
    where
        (st1, exprRes) = avaliarExpressao st expr
        (st2, finalVal) = criarValorParaAtribuicao st1 id exprRes
        (State (SymbolTable a b c d (Memory mem)) io flag) = st2
        finalMem = atribuirVar mem finalVal
        finalState = (State (SymbolTable a b c d (Memory finalMem)) io flag)

pointToId :: State -> TokenTree -> TokenTree -> State
pointToId st ptr (LeafToken (Id _ id)) = finalState
    where
        (State (SymbolTable structs procs funcs scope (Memory mem)) io flag) = st
        (Variable _ varTyp _ varSco) = lookUpScoped mem id scope
        (st2, finalVal) = criarValorParaAtribuicao st ptr ((PointerType varTyp), (Pointer id varSco))
        (State (SymbolTable a b c d (Memory nextMem)) io2 flag2) = st2
        finalMem = atribuirVar nextMem finalVal
        finalState = (State (SymbolTable a b c d (Memory finalMem)) io2 flag2)
pointToId st ptr typ = finalState
    where
        (State (SymbolTable structs procs funcs scope (Memory mem)) io flag) = st
        finalType = parseType typ
        newId = show (findNextHeapId mem)
        mem2 = instantiateNewHeapVar st mem finalType newId
        st1 = (State (SymbolTable structs procs funcs scope (Memory mem2)) io flag)
        (st2, finalVal) = criarValorParaAtribuicao st1 ptr ((PointerType finalType), (Pointer newId (Scope ("heap", -1) ("heap", -1))))
        (State (SymbolTable a b c d (Memory mem3)) io2 flag2) = st2
        finalMem = atribuirVar mem3 finalVal
        finalState = (State (SymbolTable a b c d (Memory finalMem)) io2 flag2)
--                                               id
instantiateNewHeapVar :: State -> [Variable] -> Type -> String -> [Variable]
instantiateNewHeapVar st mem typ id = (Variable id typ (defaultVal st typ) (Scope ("heap", -1) ("heap", -1))):mem

findNextHeapId :: [Variable] -> Int
findNextHeapId [] = 1
findNextHeapId ((Variable id typ val (Scope (name, depth) _)):mem) =
    if (name == "heap" && depth == -1) then
        if res > ((read id) + 1) then
            res
        else
            (read id) + 1
    else res
    where
        res = findNextHeapId mem
        
assureFlagIsNot :: FlowFlag -> [FlowFlag] -> FlowFlag
assureFlagIsNot f [] = NoneFlag
assureFlagIsNot f (nf:flags) =
    if f == nf then
        error "Comando de controle inesperado"
    else assureFlagIsNot f flags

--                           id           decls
declareStruct :: State -> TokenTree -> TokenTree -> State
declareStruct st (LeafToken (Id _ id)) decls =
    case findRes of
        Nothing -> finalState
        _ -> error ("Tentando declarar struct de nome " ++ id ++ " quando outro de mesmo nome já existe")
    where
        (State (SymbolTable structs procs funcs scope (Memory mem)) io flag) = st
        parsedDecls = parseParams decls
        findRes = findStruct structs id
        finalState = (State (SymbolTable ((Struct id parsedDecls):structs) procs funcs scope (Memory mem)) io flag)

--                            id          params         tipo        stmts
declareFunction :: State -> TokenTree -> TokenTree -> TokenTree -> TokenTree -> State
declareFunction st (LeafToken (Id _ id)) params typ stmts =
    case findRes of
        Nothing -> finalState
        _ -> error ("Tentando declarar função de nome " ++ id ++ " quando outra já existe com o mesmo nome e parâmetros")
    where
        (State (SymbolTable structs procs funcs scope (Memory mem)) io flag) = st
        retType = parseType typ
        parsedParams = parseParams params
        findRes = findFunc funcs id parsedParams
        finalState = (State (SymbolTable structs procs ((Function id retType parsedParams stmts):funcs) scope (Memory mem)) io flag)

--                            id          params         stmts
declareProcedure :: State -> TokenTree -> TokenTree -> TokenTree -> State
declareProcedure st (LeafToken (Id _ id)) params stmts =
    case findRes of
        Nothing -> finalState
        _ -> error ("Tentando declarar procedimento de nome " ++ id ++ " quando outro já existe com o mesmo nome e parâmetros")
    where
        (State (SymbolTable structs procs funcs scope (Memory mem)) io flag) = st
        parsedParams = parseParams params
        findRes = findProc procs id parsedParams
        finalState = (State (SymbolTable structs ((Procedure id parsedParams stmts):procs) funcs scope (Memory mem)) io flag)

parseParams :: TokenTree -> [Field]
parseParams None = []
parseParams (TriTree NonTVarDecls t list_ids v) =
    (parseSubParams (parseType t) list_ids) ++ (parseParams v)
parseParams (DualTree NonTVarDecls t list_ids) =
    parseSubParams (parseType t) list_ids

parseSubParams :: Type -> TokenTree -> [Field]
parseSubParams typ (DualTree NonTListIds (LeafToken (Id _ id)) list) =
    ((Field typ id):(parseSubParams typ list))
parseSubParams typ (LeafToken (Id _ id)) = [(Field typ id)]
parseSubParams typ _ = error ("Atribuições e parâmetros default não são permitidos")

evalArgs :: State -> TokenTree -> (State, [(Type, Value)])
evalArgs st (DualTree NonTParams expr next) = res
    where
        (st1, argRes) = avaliarExpressao st expr
        (finalState, otherArgs) = evalArgs st1 next
        res = (finalState, (argRes:otherArgs))
evalArgs st (UniTree NonTParams expr) = (finalState, [argRes])
    where
        (finalState, argRes) = avaliarExpressao st expr

--                       procname        params
startProcedure :: State -> String -> [(Type, Value)] -> State
startProcedure (State (SymbolTable structs procs funcs oldScope (Memory mem)) io flag) procName params =
    runProcedure (State (SymbolTable structs procs funcs oldScope (Memory mem)) io flag) proc params
    where
        -- acha o proc
        proc = findProcWrapper procs procName (fieldfy params)

runProcedure :: State -> Procedure -> [(Type, Value)] -> State
runProcedure (State (SymbolTable structs procs funcs oldScope (Memory mem)) io flag) (Procedure procName fields procTree) params =
    finalState
    where
        -- gera proximoescopo
        newStartingScopeDepth = findNextScopeDepthFromMem mem procName
        newStartingScope = Scope (procName, newStartingScopeDepth) (procName, 1)
        -- instancia coisas do proc
        newMem = instanciarParams mem params fields newStartingScope
        -- executa o proc no analisadorSemantico
        (State (SymbolTable _ _ _ _ (Memory procFinalMem)) finalIo nextFlag) = analisadorSemantico procTree (State (SymbolTable structs procs funcs [newStartingScope] (Memory newMem)) io flag)
        -- deletar coisas do escopo aqui
        finalMem = cleanScopeFromMem procFinalMem newStartingScope
        -- testar se flag está correta e em seguida resetar qualquer flag que houver
        finalFlag = assureFlagIsNot nextFlag [ContinueFlag, BreakFlag]
        -- estadoFinal
        finalState = (State (SymbolTable structs procs funcs oldScope (Memory finalMem)) finalIo finalFlag)
runProcedure st (SysProcedure procName fields func) params =
    func st params

--                         funcName        params
startFunction :: State -> TokenTree -> [(Type, Value)] -> (State, (Type, Value))
startFunction (State (SymbolTable structs procs funcs oldScope (Memory mem)) io flag) (LeafToken (Id _ funcName)) params =
    runFunction (State (SymbolTable structs procs funcs oldScope (Memory mem)) io flag) foundFunc params
    where
        -- acha a func
        foundFunc = findFuncWrapper funcs funcName (fieldfy params)
runFunction :: State -> Function -> [(Type, Value)] -> (State, (Type, Value))
runFunction (State (SymbolTable structs procs funcs oldScope (Memory mem)) io flag) (Function funcName retType fields funcTree) params =
    res
    where
        -- gera proximoescopo
        newStartingScopeDepth = findNextScopeDepthFromMem mem funcName
        newStartingScope = Scope (funcName, newStartingScopeDepth) (funcName, 1)
        -- instancia coisas da func
        newMem = instanciarParams mem params fields newStartingScope
        -- executa a func no analisadorSemantico
        (State (SymbolTable _ _ _ _ (Memory funcFinalMem)) finalIo nextFlag) = analisadorSemantico funcTree (State (SymbolTable structs procs funcs [newStartingScope] (Memory newMem)) io flag)
        -- Tentar pegar o retorno aqui
        returnedVal = getReturn funcFinalMem newStartingScope retType;
        -- deletar return aqui
        afterReturnDelMem = cleanScopeFromMem funcFinalMem (Scope (funcName, newStartingScopeDepth) ("return", 0))
        -- deletar coisas do escopo aqui
        finalMem = cleanScopeFromMem afterReturnDelMem newStartingScope
        -- valor de retorno com flag nula
        res = ((State (SymbolTable structs procs funcs oldScope (Memory finalMem)) finalIo NoneFlag), returnedVal)
        -- tempThing = fieldfy params
runFunction st (SysFunction funcName retType fields func) params =
    func st params

setReturnVal :: State -> TokenTree -> State
setReturnVal (State (SymbolTable a b c d (Memory mem)) io _) None = (State (SymbolTable a b c d (Memory mem)) io ReturnFlag) 
setReturnVal st expr = finalState
    where
        (State (SymbolTable a b c d (Memory mem)) io _) = st
        (State (SymbolTable _ _ _ scopes (Memory nextMem)) nextIO _, (exprTyp, exprVal)) = avaliarExpressao st expr;
        (Scope funcScope _) = head scopes
        finalMem = instanciarVar nextMem (Variable "return" exprTyp exprVal (Scope funcScope ("return", 0)))
        finalState = (State (SymbolTable a b c d (Memory finalMem)) nextIO ReturnFlag)

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
startBlock stmts (State (SymbolTable a b c scopes mem) io flag) blocName = finalState
    where

        (Scope (func, funcDepth) (_, _)) = head scopes
        nextScope = (Scope (func, funcDepth) (blocName, findNextScopeDepthFromScope scopes blocName))
        (State (SymbolTable _ _ _ _ (Memory afterMem)) finalIO finalFlag) = analisadorSemantico stmts (State (SymbolTable a b c (nextScope:scopes) mem) io flag)
        finalMem = cleanScopeFromMem afterMem nextScope
        finalState = (State (SymbolTable a b c scopes (Memory finalMem)) finalIO finalFlag)

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
findNextScopeDepthFromScope ((Scope (_,_) (scName, scDepth)):scopes) name =
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
        a -> error (show a)
    DualTree NonTExpr (LeafToken (SymBoolNot _)) b -> res
        where
            (st1, (type1, val1)) = avaliarExpressao st b
            res = (st1, exprBoolNot (type1, val1))
        
    DualTree NonTInvokeFunctionArgs a b -> res
        where
            -- (State, [(Type, Value)])
            (st1, args) = evalArgs st b
            res = startFunction st1 a args -- modificar para funçao
    TriTree nonT a b c -> case nonT of
        NonTExpr -> triTreeExprParser st b a c

avaliarExpressaoParseId :: State -> TokenTree -> (State, (Type, Value))
avaliarExpressaoParseId (State (SymbolTable a b c scopes (Memory mem)) io flag) (LeafToken (Id _ id)) = res
    where
        (Variable _ typ val _) = lookUpScoped mem id scopes
        res = ((State (SymbolTable a b c scopes (Memory mem)) io flag), (typ, val))
avaliarExpressaoParseId (State (SymbolTable a b c scopes (Memory mem)) io flag) (DualTree NonTIdModifiers (LeafToken (Id _ id)) mods) = res
    where
        (Variable _ typ val _) = lookUpScoped mem id scopes
        res = resolveValModifiers (State (SymbolTable a b c scopes (Memory mem)) io flag) (typ, val) mods
avaliarExpressaoParseId st (UniTree NonTPtrOp a) = dereferencePtr (avaliarExpressaoParseId st a)

resolveValModifiers :: State -> (Type, Value) -> TokenTree -> (State, (Type, Value))
resolveValModifiers st ((ListType typ), val) (DualTree NonTAccessArray expr rest) = res
    where
        (st1, (exprTyp, exprVal)) = avaliarExpressao st expr
        resVal = accessListAt val exprVal
        res = resolveValModifiers st1 (typ, resVal) rest
resolveValModifiers st ((StructType typ), (StructVal val)) (DualTree NonTAccessStruct (LeafToken (Id _ id)) rest) = res
    where 
        resVal = accessStructAt val id
        resTyp = accessStructTypeAt val id
        res = resolveValModifiers st (resTyp, resVal) rest
resolveValModifiers st vals None = (st, vals)

dereferencePtr :: (State, (Type, Value)) -> (State, (Type, Value))
dereferencePtr ((State (SymbolTable a b c d (Memory mem)) io flag), (_, Pointer id esc)) = res
    where
        (Variable _ typ val _) = lookUpWrapper mem id esc
        res = ((State (SymbolTable a b c d (Memory mem)) io flag), (typ, val))


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

exprBoolNot :: (Type, Value) -> (Type, Value)
exprBoolNot (BoolType, Bool a) = (BoolType, Bool (not a))
exprBoolNot a = error ("Operação com o tipo " ++ (show a) ++ " não é permitida")

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

findProcWrapper :: [Procedure] -> String -> [Field] -> Procedure
findProcWrapper procs name fields =
    case res of
        Nothing -> error "Procedimento não declarado"
        Just p -> p
    where
        res = findProc procs name fields

--         procedimentos    nome      tipos
findProc :: [Procedure] -> String -> [Field] -> Maybe Procedure
findProc [] _ _ = Nothing
findProc ((Procedure procName procFields tree):procs) tarName tarFields =
    if procName == tarName && procFields == tarFields
        then Just (Procedure procName procFields tree)
    else findProc procs tarName tarFields
findProc ((SysProcedure procName procFields func):procs) tarName tarFields =
    if procName == tarName && procFields == tarFields
        then Just (SysProcedure procName procFields func)
    else findProc procs tarName tarFields

findFuncWrapper :: [Function] -> String -> [Field] -> Function
findFuncWrapper funcs name fields =
    case res of
        Nothing -> error "Função não declarada"
        Just f -> f
    where
        res = findFunc funcs name fields

--            funcoes      nome      tipos
findFunc :: [Function] -> String -> [Field] -> Maybe Function
findFunc [] _ _ = Nothing
findFunc ((Function funcName retType funcFields tree):funcs) tarName tarFields =
    if funcName == tarName && funcFields == tarFields
        then Just (Function funcName retType funcFields tree)
    else findFunc funcs tarName tarFields
findFunc ((SysFunction funcName retType funcFields func):funcs) tarName tarFields =
    if funcName == tarName && funcFields == tarFields
        then Just (SysFunction funcName retType funcFields func)
    else findFunc funcs tarName tarFields

--            structs      nome
findStructWrapper :: [Struct] -> String -> Struct
findStructWrapper structs name =
    case res of
        Nothing -> error "Struct não declarado"
        Just f -> f
    where
        res = findStruct structs name

--            structs      nome
findStruct :: [Struct] -> String -> Maybe Struct
findStruct [] _ = Nothing
findStruct ((Struct name fields):structs) tarName =
    if name == tarName
        then Just (Struct name fields)
    else findStruct structs name

-- TODO: NÃO TESTADA
-- Se os tipos forem incorretos joga erro
--              Lista    Index
accessListAt :: Value -> Value -> Value
accessListAt (List a) (Int i) = returnNthOfList a i
accessListAt (List a) _ = error "indice inválido, esperava inteiro"
accessListAt _ _ = error "Valor passado não é uma lista"

accessStructAt :: [FieldInstance] -> String -> Value
accessStructAt [] _ = error "Tentando acessar campo não atribuido do struct"
accessStructAt ((FieldInstance (Field _ id) val):fields) name =
    if (id == name) then
        val
    else accessStructAt fields name

accessStructTypeAt :: [FieldInstance] -> String -> Type
accessStructTypeAt [] _ = error "Tentando acessar campo não atribuido do struct"
accessStructTypeAt ((FieldInstance (Field typ id) _):fields) name =
    if (id == name) then
        typ
    else accessStructTypeAt fields name

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
criarValorParaAtribuicao st (UniTree NonTId id) (newTyp, newVal) =
    (criarValorFinal st foundVar (newTyp, newVal) mods)
    where
        (finalSt, (foundVar, mods)) = encontrarVarAlvo st id
        (State (SymbolTable a b c d (Memory mem)) io flag) = finalSt

-- IdMagico = IdNormal String | DereferenceOf IdMagico | AtIndex IdMagico Value
-- Special = Spec Variable [Int]
--              memoria      idMagico       (Variável é o alvo a ser modificado)
encontrarVarAlvo :: State -> TokenTree -> (State, (Variable, TokenTree))
-- IdBase
encontrarVarAlvo st (LeafToken (Id _ id)) = res
    where
        (State (SymbolTable _ _ _ esc (Memory mem)) _ _) = st
        var = lookUpScoped mem id esc
        res = (st, (var, None))
-- Pointers
encontrarVarAlvo st (UniTree NonTPtrOp idm) =
    case finalTyp of
        PointerType _ -> (finalSt, ((encontrarVarAlvo_decypherPtr mem (Variable newId finalTyp finalVal newEsc)), None))
        _ -> error ("Tentando dereferenciar algo que não é ponteiro" ++ (show finalVal))
    where
        (nextSt, ((Variable newId typ newVal newEsc), modifiers)) = encontrarVarAlvo st idm
        (finalSt, (finalTyp, finalVal)) = resolveValModifiers nextSt (typ, newVal) modifiers
        (State (SymbolTable _ _ _ _ (Memory mem)) _ _) = finalSt
-- Mods
encontrarVarAlvo st (DualTree NonTIdModifiers idm mods) = res
    where
        (st1, (var, prevMods)) = encontrarVarAlvo st idm
        res = (st1, (var, mods))

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

--                  memoria  Variavel com acessos | Valor a ser escrito | Variavel final
criarValorFinal :: State -> Variable -> (Type, Value) -> TokenTree -> (State, Variable)
criarValorFinal st (Variable idm expTyp currVal escm) (newTyp, newVal) None =
    if expTyp == newTyp then
        (st, (Variable idm newTyp newVal escm))
    else error ("Tentando realizar atribuição entre tipos diferentes: " ++ (show expTyp) ++ " " ++ (show newTyp))
criarValorFinal st (Variable idm (ListType expTyp) (List currVal) escm) newVals (DualTree NonTAccessArray expr next) =
    case exprTyp of
        IntType -> (stFinal, finalVar)
        _ -> error ("Tentando acessar array " ++ (show idm) ++ " com valor não inteiro")
    where
        (st1, (exprTyp, exprRes)) = avaliarExpressao st expr
        (Int i) = exprRes
        (stFinal, (Variable _ _ modVal _)) = (criarValorFinal st1 (Variable idm expTyp (returnNthOfList currVal i) escm) newVals next)
        finalVal = (List ((listUpTo currVal i) ++ [modVal] ++ (listFromToEnd currVal (i+1))))
        finalVar = (Variable idm (ListType expTyp) finalVal escm)
criarValorFinal st (Variable idm (StructType expTyp) (StructVal currVal) escm) newVals (DualTree NonTAccessStruct (LeafToken (Id _ id)) next) =
    (stFinal, finalVar)
    where
        (stFinal, (Variable _ _ modVal _)) = (criarValorFinal st (Variable idm (accessStructTypeAt currVal id) (accessStructAt currVal id) escm) newVals next)
        finalVal = (StructVal (modifyStructAt currVal id modVal))
        finalVar = (Variable idm (StructType expTyp) finalVal escm)

modifyStructAt :: [FieldInstance] -> String -> Value -> [FieldInstance]
modifyStructAt ((FieldInstance (Field typ id) oldVal):fields) tarId newVal =
    if id == tarId then
        (FieldInstance (Field typ id) newVal):fields
    else (FieldInstance (Field typ id) oldVal):(modifyStructAt fields tarId newVal)

listUpTo :: [a] -> Int -> [a]
listUpTo _ 0 = []
listUpTo (h:l) i = h:(listUpTo l (i-1))
listUpTo [] _ = error "Out of bounds"

listFromToEnd :: [a] -> Int -> [a]
listFromToEnd l 0 = l
listFromToEnd (h:l) i = listFromToEnd l (i-1)
listFromToEnd _ _ = error "Out of bounds" -- implica que a lista é vazia e indice > 0

-- TODO: NÃO TESTADA
printAll :: State -> TokenTree -> State
printAll st (DualTree NonTParams expr next) = printAll st1 next
    where
        ((State midTable midIO flag), (typ, val)) = avaliarExpressao st expr
        nextIO = midIO >> (printOne val)
        st1 = (State midTable nextIO flag)
printAll st (UniTree NonTParams expr) = (State midTable finalIO flag)
    where
        ((State midTable midIO flag), (typ, val)) = avaliarExpressao st expr
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

deletePointer :: State -> TokenTree -> State
deletePointer st id =
    case exprTyp of
        PointerType _ -> deletePointerFromHeap midState exprVal
        _ -> error "Impossível usar 'delete' em algo que não é ponteiro"
    where
        (midState, (exprTyp, exprVal)) = avaliarExpressao st id;

deletePointerFromHeap :: State -> Value -> State
deletePointerFromHeap (State (SymbolTable a b c d (Memory mem)) io flag) (Pointer id esc) =
    if (scopeName == "heap" && depth == -1)
        then (State (SymbolTable a b c d (Memory (deletePointerFromHeapMem mem id esc))) io flag)
    else error "Impossível deletar variáveis não alocadas dinamicamente"
    where
        (Scope (scopeName, depth) _) = esc

deletePointerFromHeapMem :: [Variable] -> String -> Scope -> [Variable]
deletePointerFromHeapMem ((Variable id ty val sc):mem) searchId searchSco =
    if (searchId == id && searchSco == sc) then mem
    else (Variable id ty val sc):(deletePointerFromHeapMem mem searchId searchSco)


-- TODO: NÃO TESTADA
-- Deprecated
--        memoria     id      escopo
lookUp :: Memory -> String -> Scope -> Variable
lookUp (Memory mem) id escopo =
    case lookUpAux mem id escopo of
        Nothing -> error ("Variável não declarada" ++ (show id))
        Just res -> res

--                memoria        id      escopo
lookUpWrapper :: [Variable] -> String -> Scope -> Variable
lookUpWrapper a b c = 
    case lookUpAux a b c of
        Nothing -> error ("Variável não declarada" ++ (show b))
        Just res -> res

--               memoria        id       escopo
lookUpScoped :: [Variable] -> String -> [Scope] -> Variable
lookUpScoped mem id [] = error ("Variável não declarada" ++ (show id))
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
main = do 
    args <- getArgs
    case args of
        [file] -> do 
            -- fn <- readFile file
            case unsafePerformIO (parser (getTokens file)) of
                { Left err -> print err; 
                  Right ans -> inicAnalisadorSemantico ans
                }
        _ -> do
            case unsafePerformIO (parser (getTokens "helloworld.in")) of
                { Left err -> print err; 
                  Right ans -> inicAnalisadorSemantico ans
                }


    -- case unsafePerformIO (parser (getTokens "arquivo.in")) of
    --     { Left err -> print err; 
    --       Right ans -> inicAnalisadorSemantico ans
    --     }
