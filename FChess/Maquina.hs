module Maquina where

import Estructuras
import Movimientos
import Validacion
import Data.List

-- ####################################
-- Profundidad del árbol de generación de jugadas
depth :: Int
depth = 3
-- ####################################

-- checkMate verifica si terminó el juego en jaque mate
checkMate :: Env -> Bool
checkMate env = let (c,b,q,w) = env in check c env && staleMate env

-- staleMate verifica si terminó el juego en ahogado (tablas)
staleMate :: Env -> Bool
staleMate env = null $ moveList env

-- Otorga un valor a cada pieza
value :: Square -> Int
value x = case x of
           Empty -> 0
           WP -> 10
           WN -> 25
           WB -> 30
           WR -> 50
           WQ -> 90
           WK -> 1000
           BP -> -10
           BN -> -25
           BB -> -30
           BR -> -50
           BQ -> -90
           BK -> -1000

-- Le da un puntaje a un tablero dado
eval :: Board -> Int
eval b = mate b + material b + center b + others b + develop b 

{- ##############
mate, material, center, others, develop :: Board -> Int: Son funciones que asignan valores al tablero según diversos aspectos
   ##############
-}

center :: Board -> Int
center b = let v :: Square -> Int
               v p = if p == WP then 10 else if p == BP then -10 else 0
               e4 = v $ getSquare (toPos "e4") b
               e5 = v $ getSquare (toPos "e5") b
               d4 = v $ getSquare (toPos "d4") b
               d5 = v $ getSquare (toPos "d5") b
           in e4+e5+d4+d5


others :: Board -> Int
others b = let v :: Square -> Int
               v p = if p == WP then 2 else if p == BP then -2 else 0
               a7 = v $ getSquare (toPos "a7") b
               b7 = v $ getSquare (toPos "b7") b
               c7 = v $ getSquare (toPos "c7") b
               f7 = v $ getSquare (toPos "f7") b
               g7 = v $ getSquare (toPos "g7") b
               h7 = v $ getSquare (toPos "h7") b
               a2 = v $ getSquare (toPos "a2") b
               b2 = v $ getSquare (toPos "b2") b
               c2 = v $ getSquare (toPos "c2") b
               f2 = v $ getSquare (toPos "f2") b
               g2 = v $ getSquare (toPos "g2") b
               h2 = v $ getSquare (toPos "h2") b
           in a2+b2+c2+f2+g2+h2+a7+b7+c7+f7+g7+h7

develop :: Board -> Int
develop b = let a1 = getSquare (toPos "a1") b
                b1 = getSquare (toPos "b1") b
                c1 = getSquare (toPos "c1") b
                d1 = getSquare (toPos "d1") b
                e1 = getSquare (toPos "e1") b
                f1 = getSquare (toPos "f1") b
                g1 = getSquare (toPos "g1") b
                h1 = getSquare (toPos "h1") b
                a8 = getSquare (toPos "a8") b
                b8 = getSquare (toPos "b8") b
                c8 = getSquare (toPos "c8") b
                d8 = getSquare (toPos "d8") b
                e8 = getSquare (toPos "e8") b
                f8 = getSquare (toPos "f8") b
                g8 = getSquare (toPos "g8") b
                h8 = getSquare (toPos "h8") b
                d2 = getSquare (toPos "d2") b
                e2 = getSquare (toPos "e2") b
                d7 = getSquare (toPos "d7") b
                e7 = getSquare (toPos "e7") b
                vb1 = if b1 == WN then -2 else 0
                vc1 = if c1 == WB then -2 else if c1 == WK && d1 == WR then 5 else 0
                vd1 = if d1 == WQ then -2 else if d1 == WK then -10 else 0
                ve1 = if e1 == WK then -2 else 0
                vf1 = if f1 == WB then -2 else if f1 == WK then -10 else 0
                vg1 = if g1 == WN then -2 else if g1 == WK && f1 == WR then 5 else 0
                vb8 = if b8 == BN then 2 else 0
                vc8 = if c8 == BB then 2 else if c8 == BK && d8 == BR then -5 else 0
                vd8 = if d8 == BQ then 2 else if d8 == BK then 10 else 0
                ve8 = if e8 == BK then 2 else 0
                vf8 = if f8 == BB then 2 else if f8 == BK then 10 else 0
                vg8 = if g8 == BN then 2 else if g8 == BK && f8 == BR then -5 else 0
                vd2 = if d2 == WP then -2 else 0
                ve2 = if e2 == WP then -2 else 0
                vd7 = if d7 == BP then 2 else 0
                ve7 = if e7 == BP then 2 else 0
            in vb1 + vc1 + vd1 + ve1 + vf1 + vg1 + vb8 + vc8 + vd8 + ve8 + vf8 + vg8  + vd2 + ve2 + vd7 + ve7




mate :: Board -> Int
mate b = if checkMate (White,b,0,0) then -10000 else if checkMate (Black,b,0,0) then 10000 else 0

material :: Board -> Int
material [] = 0
material (b:bs) = value b + material bs




--Genera el siguiente movimiento de la máquina
machineMove :: Env -> IO Env
machineMove env = let (mov,val) = selectMove env $ moveTree depth env
                  in do env' <- move env mov
                        return env'

-- Genera una lista con todos los movimientos habilitados en el turno actual y el valor del tablero tras realizar cada movimiento
moveList :: Env -> [(Move,Int)]
moveList (c,b,wc,bc) = let ms = moveGen 0 b (c,b,wc,bc) --lista de movimientos
                           bs = map (\x -> updateBoard x b) ms -- lista de tableros tras mover
                           vals = map eval bs  --lista de valores para cada tablero
                       in zip ms vals

-- Genera todos los movimientos habilitados en el turno actual
moveGen :: Int -> Board ->  Env -> [Move]
moveGen _ _ (c,[],wc,bc) = []
moveGen pos orig (c,b:bs,wc,bc) = case color b == c of
                                   True -> pieceGen b pos (c,orig,wc,bc) ++ moveGen (pos+1) orig (c,bs,wc,bc)
                                   False -> moveGen (pos+1) orig (c,bs,wc,bc)


e1' :: Pos
e1' = toPos "e1"
e8' :: Pos
e8' = toPos "e8"
-- Genera todos los movimientos habilitados de una pieza en el turno actual
pieceGen :: Square -> Pos -> Env -> [Move]
pieceGen WK e1' env = let poss = concat $ possible WK e1'
                          moves1 = map (\x->(e1',x,Empty)) poss
                          moves2 = [(e1',toPos "g1" ,Empty),(e1',toPos "c1" ,Empty)]
                          list = filter (isValid env) (moves1 ++ moves2)
                      in list
pieceGen BK e8' env = let poss = concat $ possible BK e8'
                          moves1 = map (\x->(e8',x,Empty)) poss
                          moves2 = [(e8',toPos "g8" ,Empty),(e8',toPos "c8" ,Empty)]
                          list = filter (isValid env) (moves1 ++ moves2)
                      in list
pieceGen WP p1 env = let fila = div p1 8
                         poss = concat $ possible WP p1
                         moves = case fila of 
                                  6 -> map (\x->(p1,x,WQ)) poss ++ map (\x->(p1,x,WR)) poss ++ map (\x->(p1,x,WN)) poss ++ map (\x->(p1,x,WB)) poss  
                                  _ -> map (\x->(p1,x,Empty)) poss
                         list = filter (isValid env) moves
                     in list
pieceGen BP p1 env = let fila = div p1 8
                         poss = concat $ possible BP p1
                         moves = case fila of 
                                  1 -> map (\x->(p1,x,WQ)) poss ++ map (\x->(p1,x,WR)) poss ++ map (\x->(p1,x,WN)) poss ++ map (\x->(p1,x,WB)) poss  
                                  _ -> map (\x->(p1,x,Empty)) poss
                         list = filter (isValid env) moves
                     in list
pieceGen piece p1 env | rest piece = let poss = concat $ possible piece p1
                                         moves = map (\x->(p1,x,Empty)) poss
                                         list = filter (isValid env) moves
                                     in list

-- Función auxiliar para hacer disjuntos los casos de las funciones pieceGEn
rest :: Square -> Bool
rest p = p /= WK && p /= BK &&  p /= WP &&  p /= BP 


-- Crea un árbol con todos los movimientos y respuestas a partir de un estado y la puntuación de cada movimiento hasta una profundidad dada
moveTree :: Int -> Env -> [GenTree (Move,Int)]
moveTree 0 _ = []
moveTree n env = let movs = moveList env
                     next :: (Move,Int) -> [GenTree (Move,Int)]
                     next (x,y) = moveTree (n-1) (move' env x)
                     trees = map (\x -> Gen x (next x)) movs 
                 in trees

-- Selecciona un movimiento entre los mejores posibles que se han generado
selectMove :: Env -> [GenTree (Move,Int)] -> (Move,Int)
selectMove (c,_,_,_) (g:gs) = head $ nextMove c (g:gs)
selectMove _ [] = error "No hay movimientos" --nunca deberia ocurrir porque se checkea que haya movimientos antes de llamar a esta función

-- Genera la lista de los mejores movimientos habilitados
nextMove :: Color -> [GenTree (Move,Int)] -> [(Move,Int)]
nextMove _ [] = []
nextMove c (g:gs) = best c (minimax c g) (nextMove c gs)


-- Teniendo un subárbol de respuestas aplica el algoritmo minimax y calcula el mejor valor del movimiento raíz considerando las mejores respuestas del adversario
minimax :: Color  -> GenTree (Move,Int) -> (Move,Int)
minimax _ (Gen (m,i) []) = (m,i)  
minimax c (Gen g gs) = let (m,i) = g
                           gs' = map (minimax (opColor c)) gs
                           minMax :: Color -> [(Move,Int)] -> Int
                           minMax c [] = if c == White then -10000 else 10000
                           minMax c ((_,v):vs) = case c of
                                                   White -> max v $ minMax White vs
                                                   Black -> min v $ minMax Black vs
                           i' = minMax (opColor c) gs'
                       in (m,i')

-- Obtiene la lista de los mejores movimientos
best :: Color -> (Move,Int) -> [(Move,Int)] -> [(Move,Int)]
best _ (m,i) [] = [(m,i)]
best White (m,i) (x:xs) = let (m',i') = x
                          in if i'< i then [(m,i)] --no hay un mejor movimiento en la lista
                              else if i' == i then (m,i):(x:xs) --el movimiento es tan bueno como el mejor actual
                                    else x:xs -- los movimientos en la lista son mejores que el actual
best Black (m,i) (x:xs) = let (m',i') = x
                          in if i'> i then [(m,i)] --no hay un mejor movimiento en la lista
                               else if i' == i then (m,i):(x:xs) --el movimiento es tan bueno como el mejor actual
                                      else x:xs -- los movimientos en la lista son mejores que el actual
