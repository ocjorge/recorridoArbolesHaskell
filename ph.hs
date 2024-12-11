{- Proyecto Haskell - Programación Lógica y Funcional 11/12/2024 -}

{-
    Módulo principal para el proyecto Haskell de la asignatura de Programación Lógica y Funcional.
    Este módulo contiene la implementación de un intérprete para expresiones matemáticas
    que permite parsear, evaluar y mostrar expresiones matemáticas en notación infija y
    polaca, además de mostrar el árbol de la expresión.
-}
{-
Integrantes:
- 22280692 - Ortiz Ceballos Jorge
- 22280639 - Rivera Cruz Luis Antonio
-}

import Data.Char (isDigit)
import Text.Read (readMaybe)
import System.IO

-- Definición de tipos de datos para expresiones
data Expresion =
    Numero Double
    | Suma Expresion Expresion
    | Resta Expresion Expresion
    | Multiplicacion Expresion Expresion
    | Division Expresion Expresion
    deriving (Show)

{-
La implementacion de los métotods parsing recursivo descendente que
maneja la precedencia de operadores de manera efectiva.
Esto se logra a través de las funciones parseExpresion,
parseTerm y parseFactor, que manejan diferentes niveles de precedencia:
    {- -parseFactor: Maneja los elementos de mayor precedencia (números y expresiones entre paréntesis)
       -parseTerm: Maneja multiplicación y división (con mayor precedencia que suma y resta)
       -parseExpresion: Maneja suma y resta (con menor precedencia)-}
-}
-- Función principal para parsear expresiones
parsearExpresion :: String -> Maybe Expresion
parsearExpresion str = case parseExpresion (filter (/= ' ') str) of -- para filtar elementos de una lista segun una condición
    (Just exp, "") -> Just exp
    (Just _, rest) -> error $ "Error al parsear la expresión: resto no esperado: " ++ rest
    (Nothing, rest) -> error $ "Error al parsear la expresión: " ++ rest -- funcion de una lista para concatenar listas

-- Parsing con precedencia de operadores
parseExpresion :: String -> (Maybe Expresion, String) --Maneja la precedencia de los operadores de suma y resta
parseExpresion input =
    let (term, rest) = parseTerm input --Maneja la precedencia de los operadores de multiplicación y división
    in case rest of
        ('+':rest') -> let (expr, rest'') = parseExpresion rest'
                       in (Suma <$> term <*> expr, rest'')
        ('-':rest') -> let (expr, rest'') = parseExpresion rest'
                       in (Resta <$> term <*> expr, rest'')
        _ -> (term, rest) --recurción de la función parseExpresion

-- Parsing de términos (multiplicación y división)
parseTerm :: String -> (Maybe Expresion, String) --Maneja la precedencia de los operadores de multiplicación y división
parseTerm input =
    let (factor, rest) = parseFactor input
    in case rest of
        ('*':rest') -> let (term, rest'') = parseTerm rest'
                       in (Multiplicacion <$> factor <*> term, rest'')
        ('/':rest') -> let (term, rest'') = parseTerm rest'
                       in (Division <$> factor <*> term, rest'')
        _ -> (factor, rest) --recurción de la función parseTerm

-- Parsing de factores (números y paréntesis)
parseFactor :: String -> (Maybe Expresion, String) --Maneja los números y las expresiones entre paréntesis
parseFactor ('(':rest) =
    let (expr, rest') = parseExpresion rest
    in case rest' of
        (')':rest'') -> (expr, rest'')
        _ -> (Nothing, "Error: paréntesis no cerrado")
parseFactor input =
    let (num, rest) = span (\c -> Data.Char.isDigit c || c == '.') input -- Span función de lista para dividir en dos listas según condición
    in case readMaybe num :: Maybe Double of
        Just n -> (Just (Numero n), rest)
        Nothing -> (Nothing, "Error: número inválido")

-- Función para evaluar una expresión
evaluarExpresion :: Expresion -> Double --Evalúa una expresión matemática. //  se llama recursivamente para calcular el valor de expresiones compleja
evaluarExpresion (Numero x) = x
evaluarExpresion (Suma e1 e2) = evaluarExpresion e1 + evaluarExpresion e2
evaluarExpresion (Resta e1 e2) = evaluarExpresion e1 - evaluarExpresion e2
evaluarExpresion (Multiplicacion e1 e2) = evaluarExpresion e1 * evaluarExpresion e2
evaluarExpresion (Division e1 e2)
    | evaluarExpresion e2 /= 0 = evaluarExpresion e1 / evaluarExpresion e2
    | otherwise = error "División por cero"

-- Función para convertir una expresión a notación polaca (prefija)
aNotacionPolaca :: Expresion -> String
aNotacionPolaca (Numero x) = show x
-- Uso de concatenación de listas para construir notación polaca
-- La función recursiva genera una lista de strings que se concatenan
aNotacionPolaca (Suma e1 e2) = "+ " ++ aNotacionPolaca e1 ++ " " ++ aNotacionPolaca e2
aNotacionPolaca (Resta e1 e2) = "- " ++ aNotacionPolaca e1 ++ " " ++ aNotacionPolaca e2
aNotacionPolaca (Multiplicacion e1 e2) = "* " ++ aNotacionPolaca e1 ++ " " ++ aNotacionPolaca e2
aNotacionPolaca (Division e1 e2) = "/ " ++ aNotacionPolaca e1 ++ " " ++ aNotacionPolaca e2
-- Comentario: Uso de ++ para concatenar listas de strings, generando notación polaca recursivamente

-- Función para convertir una expresión a notación infija
aNotacionInfija :: Expresion -> String
aNotacionInfija (Numero x) = show x
aNotacionInfija (Suma e1 e2) = "(" ++ aNotacionInfija e1 ++ " + " ++ aNotacionInfija e2 ++ ")"
aNotacionInfija (Resta e1 e2) = "(" ++ aNotacionInfija e1 ++ " - " ++ aNotacionInfija e2 ++ ")"
aNotacionInfija (Multiplicacion e1 e2) = "(" ++ aNotacionInfija e1 ++ " * " ++ aNotacionInfija e2 ++ ")"
aNotacionInfija (Division e1 e2) = "(" ++ aNotacionInfija e1 ++ " / " ++ aNotacionInfija e2 ++ ")"

-- Resolución paso a paso en notación polaca
resolverPasoAPasoPolaca :: Expresion -> [String]
-- Retorna una lista de strings con los pasos de resolución
resolverPasoAPasoPolaca exp =
    let pasos = generarPasosDetallados exp
    -- map: Función de transformación de listas
    -- Convierte cada paso a su representación de string
    in map formatearPasoPolaca pasos -- función de lista para aplicar una función a cada elemento de una lista
    -- Comentario: map aplica formatearPasoPolaca a cada elemento de pasos,
    -- generando una nueva lista de strings

-- Formateo de paso para notación polaca
formatearPasoPolaca :: (Expresion, Double, String) -> String
formatearPasoPolaca (_, _, paso) = paso

-- Resolución paso a paso en notación infija
resolverPasoAPasoInfija :: Expresion -> [String]
resolverPasoAPasoInfija exp =
    let pasos = generarPasosDetallados exp
    in map formatearPasoInfijo pasos

-- Generación de pasos detallados
generarPasosDetallados :: Expresion -> [(Expresion, Double, String)] -- se llama recursivamente para cada subexpresión
-- Retorna una lista de tuplas con información detallada de cada paso
generarPasosDetallados exp =
    case exp of
        Numero n -> [(exp, n, "Número inicial: " ++ show n)] -- Lista con un único elemento (paso inicial)
        Suma e1 e2 ->
            let pasosE1 = generarPasosDetallados e1  -- Generación recursiva de listas de pasos para subexpresiones, aplica para todos los casos
                pasosE2 = generarPasosDetallados e2
                valorE1 = evaluarExpresion e1
                valorE2 = evaluarExpresion e2
                valorFinal = valorE1 + valorE2
                pasoSuma = (exp, valorFinal,
                    "Suma: " ++ show valorE1 ++ " + " ++ show valorE2 ++ " = " ++ show valorFinal)
            in pasosE1 ++ pasosE2 ++ [pasoSuma] -- Concatenación de listas de pasos // ++ une las listas de pasos de subexpresiones con el paso de suma // aplica para todos los casos
        Resta e1 e2 ->
            let pasosE1 = generarPasosDetallados e1 --
                pasosE2 = generarPasosDetallados e2
                valorE1 = evaluarExpresion e1
                valorE2 = evaluarExpresion e2
                valorFinal = valorE1 - valorE2
                pasoResta = (exp, valorFinal,
                    "Resta: " ++ show valorE1 ++ " - " ++ show valorE2 ++ " = " ++ show valorFinal)
            in pasosE1 ++ pasosE2 ++ [pasoResta]
        Multiplicacion e1 e2 ->
            let pasosE1 = generarPasosDetallados e1
                pasosE2 = generarPasosDetallados e2
                valorE1 = evaluarExpresion e1
                valorE2 = evaluarExpresion e2
                valorFinal = valorE1 * valorE2
                pasoMultiplicacion = (exp, valorFinal,
                    "Multiplicación: " ++ show valorE1 ++ " * " ++ show valorE2 ++ " = " ++ show valorFinal)
            in pasosE1 ++ pasosE2 ++ [pasoMultiplicacion]
        Division e1 e2 ->
            let pasosE1 = generarPasosDetallados e1
                pasosE2 = generarPasosDetallados e2
                valorE1 = evaluarExpresion e1
                valorE2 = evaluarExpresion e2
                valorFinal = valorE1 / valorE2
                pasoDivision = (exp, valorFinal,
                    "División: " ++ show valorE1 ++ " / " ++ show valorE2 ++ " = " ++ show valorFinal)
            in pasosE1 ++ pasosE2 ++ [pasoDivision]

-- Formateo de paso para notación infija
formatearPasoInfijo :: (Expresion, Double, String) -> String
formatearPasoInfijo (_, _, paso) = paso

-- Funciones para manejar archivos
leerArchivo :: FilePath -> IO String
leerArchivo path = readFile path

escribirArchivo :: FilePath -> String -> IO ()
escribirArchivo path contenido = writeFile path contenido

modificarArchivo :: FilePath -> (String -> String) -> IO ()
modificarArchivo path f = do
    contenido <- readFile path
    let nuevoContenido = f contenido
    writeFile path nuevoContenido

-- Función para mostrar el árbol de manera visual
mostrarArbol :: Expresion -> String
mostrarArbol = mostrarArbolAux 0
  where
    mostrarArbolAux :: Int -> Expresion -> String --usa recursividad para recorrer y mostrar la estructura del árbol de expresiones
    mostrarArbolAux nivel (Numero x) = indentar nivel ++ "Numero: " ++ show x
    mostrarArbolAux nivel (Suma e1 e2) =
        indentar nivel ++ "Suma:\n"
        ++ mostrarArbolAux (nivel + 1) e1 ++ "\n"
        ++ mostrarArbolAux (nivel + 1) e2
    mostrarArbolAux nivel (Resta e1 e2) =
        indentar nivel ++ "Resta:\n"
        ++ mostrarArbolAux (nivel + 1) e1 ++ "\n"
        ++ mostrarArbolAux (nivel + 1) e2
    mostrarArbolAux nivel (Multiplicacion e1 e2) =
        indentar nivel ++ "Multiplicación:\n"
        ++ mostrarArbolAux (nivel + 1) e1 ++ "\n"
        ++ mostrarArbolAux (nivel + 1) e2
    mostrarArbolAux nivel (Division e1 e2) =
        indentar nivel ++ "División:\n"
        ++ mostrarArbolAux (nivel + 1) e1 ++ "\n"
        ++ mostrarArbolAux (nivel + 1) e2

    -- Función para añadir indentación según el nivel
    indentar :: Int -> String
    indentar n = replicate (n * 4) ' '  -- Usa 4 espacios por nivel

-- La función main función principal que lee la expresión desde un archivo, la parsea, evalúa y muestra los resultados
-- Función main: Procesamiento de listas en la ejecución principal
main :: IO ()
main = do
    -- Leer la expresión desde un archivo de texto
    contenido <- leerArchivo "expresion.txt"

    -- Intentar parsear la expresión
    case parsearExpresion contenido of
        Just expresion -> do  -- Generación de listas de resultados paso a paso
            let resultadoInfija = aNotacionInfija expresion
            putStrLn $ "\nResultado en Notación Infija: " ++ resultadoInfija
            putStrLn ""
         
            let pasosInfijos = resolverPasoAPasoInfija expresion
            -- mapM_: Ejecución de acción (putStrLn) para cada elemento de lista
            putStrLn "Resolución Paso a Paso - Notación Infija:"
            mapM_ putStrLn pasosInfijos
            putStrLn ""


            let resultadoPolaca = aNotacionPolaca expresion
            putStrLn "\nResultado en Notación Polaca:"
            putStrLn resultadoPolaca
            putStrLn ""

            let pasosPolacos = resolverPasoAPasoPolaca expresion
            putStrLn "\nResolución Paso a Paso - Notación Polaca:"
            mapM_ putStrLn pasosPolacos
            putStrLn ""

            -- Mostrar el árbol de la expresión
            putStrLn "Árbol de la expresión:"
            putStrLn $ mostrarArbol expresion
            putStrLn ""

            let resultadoFinal = evaluarExpresion expresion
            putStrLn $ "\nResultado Final (Notación Infija): " ++ show resultadoFinal
            putStrLn $ "Resultado Final (Notación Polaca): " ++ show resultadoFinal

            

            -- Escribir los resultados en archivos
            -- unlines: Convierte lista de strings en un único string con saltos de línea
            escribirArchivo "resultado_infijo.txt" (unlines pasosInfijos)
            escribirArchivo "resultado_polaco.txt" (resultadoPolaca ++ "\n" ++ unlines pasosPolacos)

        Nothing -> putStrLn "Error al parsear la expresión."

{-
RESUMEN DE USO DE LISTAS:
1. Generación recursiva de listas
2. Transformación de listas con map
3. Concatenación de listas con ++
4. Procesamiento de listas con mapM_
5. Conversión de listas a strings con unlines
-}

-- Lista de funciones implementadas
-- 1. parsearExpresion: Función principal para parsear expresiones.
-- 2. parseExpresion: Parsing con precedencia de operadores.
-- 3. parseTerm: Parsing de términos (multiplicación y división).
-- 4. parseFactor: Parsing de factores (números y paréntesis).
-- 5. evaluarExpresion: Función para evaluar una expresión.
-- 6. aNotacionPolaca: Función para convertir una expresión a notación polaca (prefija).
-- 7. resolverPasoAPasoPolaca: Resolución paso a paso en notación polaca.
-- 8. formatearPasoPolaca: Formateo de paso para notación polaca.
-- 9. resolverPasoAPasoInfija: Resolución paso a paso en notación infija.
-- 10. generarPasosDetallados: Generación de pasos detallados.
-- 11. formatearPasoInfijo: Formateo de paso para notación infija.
-- 12. leerArchivo: Función para leer un archivo.
-- 13. escribirArchivo: Función para escribir en un archivo.
-- 14. modificarArchivo: Función para modificar un archivo.
-- 15. mostrarArbol: Función para mostrar el árbol de manera visual.
-- 16. main: Función principal del programa.
