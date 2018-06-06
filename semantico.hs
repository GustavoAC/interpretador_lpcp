import Sintatico
import Lexico

import System.IO.Unsafe


-- ListaDeStructs(nome [campos (nome typo)]) ListaDeProcedimentos(Id Parâmetros BlocoDaFunção) ListaDeFunções(Id TipodeRetorno Parâmetros BlocoDaFunção) ListadeEscopos ListaDeVariáveis(Id Tipo Valor Escopo)
data State = State SymbolTable (IO ())
data SymbolTable = SymbolTable [Struct] [Procedure] [Function] [String] Memory deriving (Eq, Show)
data Procedure = Procedure String [Field] TokenTree deriving (Eq, Show)
data Function = Function String Type [Field] TokenTree deriving (Eq, Show)
data Struct = Struct String [Field] deriving (Eq, Show)
data Field = Field Type String deriving (Show)
-- Basta que os tipos sejam iguais para um field ser igual a outro.
instance Eq Field where
    (Field name1 type1) == (Field name2 type2) = (type1 == type2)

data Memory = Memory [Variable] deriving (Eq, Show)
data Variable = Variable String Type Value String deriving (Eq, Show)

data Type = IntType | FloatType | StringType | BoolType | ListType Type | PointerType Type | StructType String deriving (Eq, Show)
data Value = Int Int |
             Float Float |
             String String |
             Bool Bool |
             List [Value] |
--                     id   escopo
             Pointer String String |
             StructVal [FieldInstance] deriving (Eq, Show)
data FieldInstance = FieldInstance Field Value deriving (Eq, Show)


-- Teste
data IdLabelBeta = IdBaseBeta String | DereferenceOfBeta IdLabelBeta | AtIndexBeta IdLabelBeta Value

emptyState :: State
emptyState = State (SymbolTable [] [] [] [] (Memory [])) (return ())

inicAnalisadorSemantico :: TokenTree -> IO()
inicAnalisadorSemantico tree = getAnalisadorIO (analisadorSemantico tree emptyState)

getAnalisadorIO :: State -> IO()
getAnalisadorIO (State _ io) = io

analisadorSemantico :: TokenTree -> State -> State
-- analisadorSemantico (TriTree NonTAssign a b c) (State table io) =
--    State (atribuir table) ((print "seraaaa") >> io)

-- printando a tabela ao fim da execucao
analisadorSemantico (UniTree NonTProgram a) (State table io) = 
    State table2 ((print table2) >> io2)
    where
        (State table2 io2) = analisadorSemantico a (State table io)

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
analisadorSemantico (DualTree _ a b) st =
    analisadorSemantico b st1
    where
       st1 = analisadorSemantico a st
analisadorSemantico (UniTree _ a) st = 
    analisadorSemantico a st
analisadorSemantico (None) st = st

-- TODO: TESTAR FAMÍLIA DE FUNÇÕES STARTPROCEDURE
--       Trocar params de [String] pra [(Type, Value)] e avaliar por aqui
--       Se cada valor passado é um id ou uma expressão.
--                       procname    params
startProcedure :: State -> String -> [(Type, Value)] -> State
startProcedure (State (SymbolTable structs procs funcs oldScope (Memory mem)) io) procName params =
    finalState
    where
        -- acha o proc
        (Procedure _ fields procTree) = findProc procs procName (fieldfy mem params oldScope)
        -- instancia coisas do proc
        newMem = instanciarParams mem params fields procName
        -- executa o proc no analisadorSemantico
        (State (SymbolTable _ _ _ _ (Memory procFinalMem)) finalIo) = analisadorSemantico procTree (State (SymbolTable structs procs funcs [procName] (Memory newMem)) io)
        -- deletar coisas do escopo aqui
        finalMem = cleanScopeFromMem procFinalMem procName
        -- temp só pra compilar
        finalState = (State (SymbolTable structs procs funcs oldScope (Memory finalMem)) finalIo)

cleanScopeFromMem :: [Variable] -> String -> [Variable]
cleanScopeFromMem [] _ = []
cleanScopeFromMem ((Variable id typ val valEsc):mem) esc =
    if valEsc == esc
        then cleanScopeFromMem mem esc
    else cleanScopeFromMem ((Variable id typ val valEsc):mem) esc

-- TODO: Substituir os _ por NonTExpr
-- Como funções podem ser chamadas de dentro de expressões e funções podem modificar o
-- estado, então o estado todo tem que ser passado pra o avaliador de expr
--                 estado    arvoreExpr   estadofinal e valor encontrado
avaliarExpressao :: State -> TokenTree -> (State, (Type, Value))
avaliarExpressao st tree = case tree of
    UniTree nonT a -> case nonT of
        _ -> case a of 
            _ -> (st, (IntType, (Int 4)))
    DualTree nonT a b -> case nonT of
        _ -> case a of 
            _ -> (st, (IntType, (Int 4)))
    TriTree nonT a b c -> case nonT of
        _ -> case b of 
            (LeafToken (SymOpPlus _ _)) -> res
            where
                (st1, (type1, val1)) = avaliarExpressao st a
                (st2, (type2, val2)) = avaliarExpressao st1 c
                res = (st2, exprSum (type1, val1) (type2, val2))

exprSum :: (Type, Value) -> (Type, Value) -> (Type, Value)
exprSum (IntType, Int a) (IntType, Int b) = (IntType, Int (a + b))
exprSum _ _ = error "Operação entre tipos não permitida"

--                   memoria       params             campos    escopo    memoria atualizada
instanciarParams :: [Variable] -> [(Type, Value)] -> [Field] -> String -> [Variable]
instanciarParams mem ((typeValue, value):params) ((Field typeField id):fields) escopo = finalMem
    where
        nextMem = ((Variable id typeField value escopo):mem)
        finalMem = (instanciarParams nextMem params fields escopo)
instanciarParams mem [] [] _ = mem
instanciarParams _ _ _ _ = error "Erro inesperado em instanciarParams"

--          memory        params      scope
fieldfy :: [Variable] -> [(Type, Value)] -> [String] -> [Field]
fieldfy mem [] scope = []
fieldfy mem ((t, v):params) scope = f:(fieldfy mem params scope)
    where
        f = Field t []

--         procedimentos    nome      tipos
findProc :: [Procedure] -> String -> [Field] -> Procedure
findProc [] _ _ = error "Procedimento não declarado"
findProc ((Procedure procName procFields tree):procs) tarName tarFields =
    if procName == tarName && procFields == tarFields
        then (Procedure procName procFields tree)
    else findProc procs tarName tarFields


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
--             memoria  id/tipo/escopo/valor
instanciar :: [Variable] -> Variable -> [Variable]
instanciar mem (Variable id typ val sc) =
    if (lookUpAux mem id sc) == Nothing
        then (Variable id typ val sc):mem
    else error "Variável tentando ser redeclarada"

-- TODO: NÃO TESTADA 
--           memoria  id/tipo/escopo/valor
atribuir :: [Variable] -> Variable -> [Variable]
atribuir [] _ = error "Variável não declarada"
atribuir ((Variable vId vTyp vVal vSc):mem) (Variable id typ val sc) =
    if vId == id && vSc == sc
        then if vTyp == typ
            then ((Variable vId vTyp val vSc):mem)
        else 
            error "Tipo incorreto"
    else 
        (Variable vId vTyp vVal vSc):(atribuir mem (Variable id typ val sc))

-- TODO: Testar toda a família de funções criarValorParaAtribuicao
-- Essa função faz um abuso do tipo Variable, reutilizando seus campos pra armazenar
-- os valores de uma forma diferente da semântica original atribuida a ela.
-- Em [parte1] o tipo é na verdade o tipo esperado do variável, enquanto valor é o
-- valor final que será atribuído a ela e id/escopo se referem ao id/escopo da
-- variável alvo real.

-- Essa função será chamada pela parte principal do programa com o valor final determinado
-- pela resolução de uma expressão. Aqui a variável tentará ser criada para ser posta na
-- memória e após isso basta chamar a função de atribuição nela
--                            memoria      idMagico    doquevaiseratrib escopoatual
criarValorParaAtribuicao :: [Variable] -> IdLabelBeta -> Type -> Value -> String -> Variable
criarValorParaAtribuicao mem id newTyp newVal esc =
    if realTyp == newTyp
        then criarValorFinal mem ((Variable targetId realTyp finalVal targetEsc), list)
    else error "Tipos incompatíves na atribuição"
    where
        ((Variable targetId realTyp finalVal targetEsc), list) = encontrarVarAlvo mem id newVal esc


-- IdMagico = IdNormal String | DereferenceOf IdMagico | AtIndex IdMagico Value
-- Special = Spec Variable [Int]
--              memoria      idMagico  aseratrib escopoatual     (Variável carrega o tipo ESPERADO)
--                                                               (Também o valor final)
encontrarVarAlvo :: [Variable] -> IdLabelBeta -> Value -> String -> (Variable, [Value])
encontrarVarAlvo mem (IdBaseBeta id) newVal esc = var
    where
        (Variable oldId typ oldval oldEsc) = lookUpWrapper mem id esc
        var = ((Variable id typ newVal esc), [])

encontrarVarAlvo mem (DereferenceOfBeta idm) val esc =
    if checkForPtrType solvedValue
        then (encontrarVarAlvo_decypherPtr mem (Variable oldId typ solvedValue oldEsc), [])
    else error "Tentando dereferenciar algo que não é ponteiro"
    where
        ((Variable oldId typ oldVal oldEsc), oldList) = encontrarVarAlvo mem idm val esc
        solvedValue = (encontrarVarAlvo_getMatrixVal (oldVal, oldList))

encontrarVarAlvo mem (AtIndexBeta idm index) val esc =
    (encontrarVarAlvo_listAdd oldVar oldList index)
    where
        (oldVar, oldList) = encontrarVarAlvo mem idm val esc

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
-- Deprecated
--        memoria     id      escopo
lookUp :: Memory -> String -> String -> Variable
lookUp (Memory mem) id escopo =
    case lookUpAux mem id escopo of
        Nothing -> error "Variável não declarada"
        Just res -> res

--                memoria        id      escopo
lookUpWrapper :: [Variable] -> String -> String -> Variable
lookUpWrapper a b c = 
    case lookUpAux a b c of
        Nothing -> error "Variável não declarada"
        Just res -> res

--               memoria        id       escopo
lookUpScoped :: [Variable] -> String -> [String] -> Variable
lookUpScoped mem id [] = error "Variável não declarada"
lookUpScoped mem id (currEsc:esc) = 
    case lookUpAux mem id currEsc of
        Nothing -> lookUpScoped mem id esc
        Just res -> res

-- TODO: NÃO TESTADA
lookUpAux :: [Variable] -> String -> String -> Maybe Variable
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