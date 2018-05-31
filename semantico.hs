import Sintatico
import Lexico

import System.IO.Unsafe

-- ListaDeStructs ListaDeFunções(Id TipodeRetorno Parâmetros BlocoDaFunção) ListadeEscopos ListaDeVariáveis(Id Tipo Valor Escopo)
data State = State SymbolTable (IO ())
data SymbolTable = SymbolTable [String] [Function] [String] Memory deriving (Eq, Show)
data Function = Function String String String TokenTree deriving (Eq, Show)
data Memory = Memory [String] deriving (Eq, Show)
-- data Variable = Variable String String String String deriving (Eq, Show)

emptyState :: State
emptyState = State (SymbolTable [] [] [] (Memory [])) (return ())

inicAnalisadorSemantico :: TokenTree -> IO()
inicAnalisadorSemantico tree = getAnalisadorIO (analisadorSemantico tree emptyState)

getAnalisadorIO :: State -> IO()
getAnalisadorIO (State _ io) = io

analisadorSemantico :: TokenTree -> State -> State
analisadorSemantico (TriTree NonTAssign a b c) (State table io) =
    State (atribuir table) ((print "seraaaa") >> io)

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
atribuir (SymbolTable a b v (Memory mem)) = (SymbolTable a b v (Memory ("aaa":mem)))

main :: IO ()
main = case unsafePerformIO (parser (getTokens "arquivo.in")) of
            { Left err -> print err; 
              Right ans -> inicAnalisadorSemantico ans
            }