import Sintatico
import Lexico

import System.IO.Unsafe


-- ListaDeStructs(nome [campos (nome typo)]) ListaDeFunções(Id TipodeRetorno Parâmetros BlocoDaFunção) ListadeEscopos ListaDeVariáveis(Id Tipo Valor Escopo)
data State = State SymbolTable (IO ())
data SymbolTable = SymbolTable [Struct] [Function] [String] Memory deriving (Eq, Show)
data Function = Function String Type [Field] TokenTree deriving (Eq, Show)
data Struct = Struct String [Field] deriving (Eq, Show)
data Field = Field Type String deriving (Eq, Show)
data Memory = Memory [Variable] deriving (Eq, Show)
data Variable = Variable String Type (Value) String deriving (Eq, Show)

data Type = IntType | FloatType | StringType | BoolType | ListType Type | PointerType Type | StructType Struct deriving (Eq, Show)
data Value = Int Int |
             Float Float |
             String String |
             Bool Bool |
             List [Value] |
--                     id   escopo
             Pointer String String |
             StructVal [FieldInstance] deriving (Eq, Show)
data FieldInstance = FieldInstance Type Value deriving (Eq, Show)

emptyState :: State
emptyState = State (SymbolTable [] [] [] (Memory [])) (return ())

inicAnalisadorSemantico :: TokenTree -> IO()
inicAnalisadorSemantico tree = getAnalisadorIO (analisadorSemantico tree emptyState)

getAnalisadorIO :: State -> IO()
getAnalisadorIO (State _ io) = io

analisadorSemantico :: TokenTree -> State -> State
analisadorSemantico (TriTree NonTAssign a b c) (State table io) =
    State (atribuir table) ((print "seraaaa") >> io)

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



-- TODO:
-- Trocar o valor pra um tipo genérico de valor
-- Checagem de tipo
-- Esperar expressão 
atribuir :: SymbolTable -> SymbolTable
atribuir (SymbolTable a b v (Memory mem)) = (SymbolTable a b v (Memory ((Variable "teste" StringType (String "valor") "escopoTeste"):mem)))

-- TODO: NÃO TESTADA
--        memoria     id      escopo
lookUp :: Memory -> String -> String -> Maybe Variable
lookUp (Memory mem) id escopo = lookUpAux mem id escopo

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