module Main where

import Estructuras
import Validacion
import Movimientos
import Maquina
import Parsing
import Parser


main :: IO ()
main = do presentacion
          j <- config
          jugar j

--imprime mensaje de inicio
presentacion :: IO () 
presentacion = putStrLn "Bienvenid@!!\nElija el color con que desea jugar\nB: Blancas\nN: Negras\n"

-- Da a elegir con qué color jugar
config :: IO Color 
config = do str <- getLine 
            case parse side str of
             [(out,[])] -> return $ toColor out
             _ -> do putStrLn "Entrada inválida, escriba 'B' para jugar con Blancas o 'N' para jugar con negras"
                     config
             
-- Inicia el juego
jugar :: Color -> IO ()
jugar c = do putStrLn $ prettyBoard c initEnv 
             if c == White then humano c initEnv
              else maquina c initEnv
             putStrLn "Gracias por jugar!"

-- Realiza un movimiento dado por el jugador, verifica si el juego terminó y llama al siguiente movimiento de la máquina
humano :: Color -> Env -> IO ()
humano c env = do  env' <- humanMove env 
                   putStrLn $ prettyBoard c env'
                   if checkMate env' then putStrLn "Ganador: Humano\n" else if staleMate env' then putStrLn "Ahogado: Empate" else maquina c env'

-- Lee un movimiento del teclado y verifica si és válido
humanMove :: Env -> IO Env
humanMove env = do mov <- leerMovimiento 
                   if isValid env mov then move env mov else do putStrLn "Movimiento Inválido"
                                                                humanMove env                  

-- Genera un movimiento de la máquina y si no se llegó al final del juego llama al siguiente movimiento del adversariollama
maquina :: Color -> Env -> IO ()
maquina c env = do env' <- machineMove env
                   putStrLn $ prettyBoard c env'
                   if checkMate env' then putStrLn "Ganador: Máquina\n" else if staleMate env' then putStrLn "Ahogado: Empate" else humano c env'

