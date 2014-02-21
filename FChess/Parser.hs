module Parser where

import Estructuras
import Movimientos
import Parsing

leerMovimiento :: IO Move
leerMovimiento = do str <- getLine
                    case parse readMove str of
                     [(out,[])] -> return $ toMove out
                     _ -> do putStrLn "Entrada Inválida\nLos movimientos se escriben en formato ColumnaFila-ColumnaFila.\nEjemplo: Avanzar el peón rey blanco dos casillas desde su posición inicial se denota: e2e4\nPara Más información leer informe adjunto"
                             leerMovimiento

columna :: Char -> Bool
columna x = 'a' <= x && x <= 'h'

fila :: Char -> Bool
fila x = '1' <= x && x <= '8'

coronacion :: Char -> Bool
coronacion x = x == 'd' || x == 't' || x == 'a' || x == 'c'

-- Analiza si la entrada es un movimiento
readMove :: Parser String
readMove = do str <- token par1
              token (par2 str) +++ return str
              
par1 :: Parser String  
par1 = do c1 <- sat columna
          f1 <- sat fila
          c2 <- sat columna
          f2 <- sat fila
          return [c1,f1,c2,f2]

par2 :: String -> Parser String
par2 xs = do x <- sat coronacion
             return (xs++[x])

-- Analiza si la entrada es una opción válida
side :: Parser Char
side = do c <- token $ sat (\x -> x == 'B' || x == 'N')
          return c
