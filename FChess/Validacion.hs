module Validacion where

import Estructuras
import Movimientos

-- ############### VALIDACION DE UN MOVIMIENTO ############### 

-- rightColor indica si el color de una pieza a mover coincide con el color del jugador que mueve
rightColor :: Color -> Square -> Bool
rightColor c s = c == color s

-- possible nos da el conjunto de casillas a las que se puede mover una pieza desde una casilla dada (no verifica si hay algo en el camino)
possible :: Square -> Pos -> [[Pos]]
possible Empty _ = [] -- las casilla no se mueven :P
possible WR x = let f = div x 8 -- nos da la fila 
                    c = mod x 8 -- nos da la columna
                in [reverse . map (+(f*8)) $ [0..c-1],
                    map (+(f*8)) [c+1..7],
                    reverse . map (\x->x*8+c) $ [0..f-1],
                    map (\x->x*8+c) [f+1..7]
                   ]
possible BR x = possible WR x
possible WB x = let f = div x 8 -- nos da la fila 
                    c = mod x 8 -- nos da la columna
                    leftTop = min (7-f) c -- cantidad de casillas en la diagonal superior izquierda
                    rightTop = min (7-f) (7-c) -- cantidad de casillas en la diagonal superior derecha
                    leftBottom = min f c -- cantidad de casillas en la diagonal inferior izquierda
                    rightBottom = min f (7-c) -- cantidad de casillas en la diagonal inferior derecha
                in [ [x-9*i | i <- [1..leftBottom]], 
                     [x+9*i | i <- [1..rightTop]],
                     [x-7*i | i <- [1..rightBottom]],
                     [x+7*i | i <- [1..leftTop]]
                   ]
possible BB x = possible WB x
possible WQ x = possible WR x ++ possible WB x
possible BQ x = possible WQ x
possible WN x = let f = div x 8 -- nos da la fila 
                    c = mod x 8 -- nos da la columna
                    list = [[x+6], [x+15], [x+17], [x+10], [x-6], [x-15], [x-17], [x-10]]
                    p :: [Int] -> Bool
                    p [y] = let c' = mod y 8
                                f' = div y 8
                            in y >= 0 && abs (f'-f) + abs (c'-c) == 3 && y < 64
                in filter p list
possible BN x = possible WN x
possible WK x = filter (not.null) . map (take 1) $ (possible WQ x)
possible BK x = possible WK x
possible WP x = case div x 8 of
                 1 -> filter (oneRow x) [[x+7],[x+8,x+16],[x+9]]
                 _ -> filter (oneRow x)  [[x+7],[x+8],[x+9]]
possible BP x = case div x 8 of
                 6 -> filter (oneRow x) [[x-7],[x-8,x-16],[x-9]]
                 _ -> filter (oneRow x) [[x-7],[x-8],[x-9]]

-- oneRow verifica la validez de los movimientos de peón
oneRow :: Pos -> [Pos] -> Bool
oneRow x [] = True   
oneRow x (y:ys) = let f1 = div x 8
                      f2 = div y 8
                  in (abs (x-y) == 16 || abs (f1-f2) == 1) && oneRow x ys

-- Permite verificar que una pieza puede moverse a una casilla en un tablero vacío
isPossible :: [[Pos]] -> Pos -> Bool
isPossible ps p2 = any (elem p2) ps

-- Da la posición del rey de un jugador
findKing :: Color -> Board -> Pos
findKing _ [] = error "No hay rey"
findKing White (b:bs) = if b == WK then 0 else 1 + findKing White bs
findKing Black (b:bs) = if b == BK then 0 else 1 + findKing Black bs

-- check dice si el jugador dado está en jaque
check :: Color -> Env -> Bool
check c (_,b,_,_) = let k = findKing c b -- encuentra la casilla donde está en rey
                        l1 = possible WN k
                        l2 = map (map (\x -> getSquare x b)) l1
                        b1 = case c of 
                              White -> any (== [BN]) l2 -- si hay un caballo atacando al rey
                              Black -> any (== [WN]) l2 -- si hay un caballo atacando al rey
                        l3 = possible WQ k
                        l4 = map (map (\x -> getSquare x b)) l3
                        l5 = possible WK k
                        l6 = map (map (\x -> getSquare x b)) l5
                        b2 = aux c l4 l6
                    in (hayRey c b) && (b1 || b2)

-- Verifica que no se haya tomado el rey
hayRey :: Color -> Board -> Bool
hayRey _ [] = False
hayRey White (b:bs) = b == WK || hayRey White bs
hayRey Black (b:bs) = b == BK || hayRey Black bs


-- Función auxiliar para verificar que un jugador está en jaque
aux :: Color -> [[Square]] -> [[Square]] -> Bool
aux c xs ks = let [l,r,d,u,lb,rt,rb,lt] = xs
                  b1 = firstRorQ c l || firstRorQ c r || firstRorQ c d || firstRorQ c u
                  b2 = firstBorQ c lb || firstBorQ c rt || firstBorQ c rb || firstBorQ c lt
                  b3 = case c of
                        White -> pawn c rt || pawn c lt 
                        Black -> pawn c rb || pawn c lb
                  b4 = case c of
                        White -> any (==[BK]) ks
                        Black -> any (==[WK]) ks
              in b1 || b2 || b3 || b4

{- #############
firstRorQ, firstBorQ , pawn :: Color -> [Square] -> Bool: Son otras funciones auxiliares para verificar jaque
   #############
-}
firstRorQ :: Color -> [Square] -> Bool
firstRorQ _ [] = False
firstRorQ White (x:xs) = case x of
                          Empty -> firstRorQ White xs
                          BR -> True
                          BQ -> True
                          _ -> False
firstRorQ Black (x:xs) = case x of
                          Empty -> firstRorQ Black xs
                          WR -> True
                          WQ -> True
                          _ -> False

firstBorQ :: Color -> [Square] -> Bool
firstBorQ _ [] = False
firstBorQ White (x:xs) = case x of
                          Empty -> firstBorQ White xs
                          BB -> True
                          BQ -> True
                          _ -> False
firstBorQ Black (x:xs) = case x of
                          Empty -> firstBorQ Black xs
                          WB -> True
                          WQ -> True
                          _ -> False

pawn :: Color -> [Square] -> Bool
pawn _ [] = False
pawn White (x:xs) = x == BP
pawn Black (x:xs) = x == WP

-- inCheck toma el movimiento del jugador que movió y verifica que no se coloque a si mismo en jaque
inCheck :: Move -> Env -> Bool
inCheck m e = let (c,b,wc,bc) = e 
                  b' = updateBoard m b
                  e' = (c,b',wc,bc)
              in check c e'

-- Verifica que una pieza no salte sobre otras en una jugada
freeWay :: Pos -> [[Pos]] -> Board -> Bool
freeWay p ps b = let l1 = head (filter (elem p) ps) -- ps es una lista de columnas y diagonales. Buscamos en donde nos movemos
                     l2 = takeWhile (/= p) l1  -- Tomamos el tramo que queremos ver que está vacío
                     l3 = map (\x -> getSquare x b) l2 -- Vemos que hay en cada casilla
                 in all (== Empty) l3 -- vemos que esté limpio el camino

--
--
-- wc y bc son referencias para validar el enroque de las blancas y las negras respectivamente
-- Evalúa la validez de un movimiento
isValid :: Env -> Move -> Bool
-- Coronación
isValid (White,b,wc,bc) (p1, p2, cor) | cor /= Empty = let p = getSquare p1 b
                                                           p' = getSquare p2 b
                                                           fp = div p1 8
                                                       in rightColor White p && 
                                                          notKing p' &&
                                                          fp == 6 && ( -- el peón que se mueve está en septima fila
                                                           ( 
                                                            p' == Empty && -- la casilla de adelante está libre
                                                            p2-p1 == 8 -- verifica que avanaza de la septima a octaba fila en una columna
                                                           ) ||
                                                           (
                                                            not $ rightColor White p' && -- la casilla a la que avanza está ocupada por una pieza rival
                                                            (p2-p1 == 9 || p2-p1 == 7) -- se corona capturando una pieza
                                                           )
                                                          ) &&
                                                          (not $ inCheck (p1,p2,Empty) (White,b,wc,bc)) 
-- lo mismo para las negras
isValid (Black,b,wc,bc) (p1, p2, cor) | cor /= Empty = let p = getSquare p1 b
                                                           p' = getSquare p2 b
                                                           fp = div p1 8
                                                       in rightColor Black p && fp == 1 && 
                                                          notKing p' &&
                                                          ( (p' == Empty && p1-p2 == 8) || (not $ rightColor Black p' && (p1-p2 == 9 || p1-p2 == 7) )) && 
                                                          (not $ inCheck (p1,p2,Empty) (Black,b,wc,bc))
-- demás movimiento de peón
isValid (White,b,wc,bc) (p1, p2, Empty) | getSquare p1 b == WP =   notKing (getSquare p2 b) &&
                                                                   (not $ inCheck (p1,p2,Empty) (White,b,wc,bc)) && 
                                                                   div p1 8 /= 6 &&
                                                                   case p2-p1 of
                                                                    7 -> Black == color (getSquare p2 b) && div p2 8 - div p1 8 == 1 --avanza 1 fila (por los bordes)
                                                                    9 -> Black == color (getSquare p2 b) && div p2 8 - div p1 8 == 1 --avanza 1 fila
                                                                    8 -> getSquare p2 b == Empty
                                                                    16 -> div p1 8 == 1 && getSquare p2 b == Empty && getSquare (p1+8) b == Empty
                                                                    _ -> False
isValid (Black,b,wc,bc) (p1, p2, Empty) | getSquare p1 b == BP =   notKing (getSquare p2 b) &&
                                                                   (not $ inCheck (p1,p2,Empty) (Black,b,wc,bc)) && 
                                                                   div p1 8 /= 1 &&
                                                                   case p1-p2 of
                                                                    7 -> White == color (getSquare p2 b) && div p2 8 - div p1 8 == -1
                                                                    9 -> White == color (getSquare p2 b) && div p2 8 - div p1 8 == -1
                                                                    8 -> getSquare p2 b == Empty
                                                                    16 -> div p1 8 == 6 && getSquare p2 b == Empty && getSquare (p1-8) b == Empty
                                                                    _ -> False

-- wc y bc
-- 1 significa que no puede hacer enroque corto
-- 2 que no puede hacer enroque largo
-- 3 que no puede hacer ningun enroque
isValid (White,b,wc,bc) (p1, p2, Empty) | getSquare p1 b == WK = let b' = updateBoard (toMove "e1f1") b 
                                                                     b'' = updateBoard (toMove "e1d1") b
                                                                     linea1 = not $ check White (White,b',wc,bc)
                                                                     linea2 = not $ check White (White,b'',wc,bc)
                                                                     nc = (not $ check White (White,b,wc,bc)) 
                                                                 in (not $ inCheck (p1,p2,Empty) (White,b,wc,bc)) && 
                                                                    case p2-p1 of
                                                                     2 -> wc /= 1 && wc /= 3 && (getSquare (p1+1) b, getSquare (p1+2) b)==(Empty,Empty) && linea1&& nc
                                                                     -2 -> wc /= 2 && wc /= 3 && (getSquare (p1-1) b,getSquare (p1-2) b) == (Empty,Empty) && linea2&& nc && getSquare (p1-3) b == Empty
                                                                     _ -> (not $ rightColor White (getSquare p2 b)) && isPossible (possible WK p1) p2
isValid (Black,b,wc,bc) (p1, p2, Empty) | getSquare p1 b == BK = let b' = updateBoard (toMove "e8f8") b
                                                                     b'' = updateBoard (toMove "e8d8") b
                                                                     linea1 = not $ check Black (Black,b',wc,bc)
                                                                     linea2 = not $ check Black (Black,b'',wc,bc)
                                                                     nc = (not $ check Black (Black,b,wc,bc))
                                                                 in (not $ inCheck (p1,p2,Empty) (Black,b,wc,bc)) && 
                                                                    case p2-p1 of
                                                                     2 -> bc /= 1 && bc /= 3 && (getSquare (p1+1) b,getSquare (p1+2) b) == (Empty,Empty) && linea1 && nc
                                                                     -2 -> bc /= 2 && bc /= 3 && (getSquare (p1-1) b,getSquare (p1-2) b) == (Empty,Empty) && linea2&& nc && getSquare (p1-3) b == Empty
                                                                     _ -> (not $ rightColor Black (getSquare p2 b)) && isPossible (possible WK p1) p2
-- Demás movimientos
isValid (c,b,wc,bc) (p1, p2, Empty) = let p = getSquare p1 b
                                          p' = getSquare p2 b -- para verificar que no toma una pieza del mismo color
                                          poss = possible p p1
                                      in rightColor c p &&  -- verificamos color
                                         notKing p' &&
                                         (not $ rightColor c p') && -- llega a una casilla vacía o se toma una pieza rival
                                         (not $ inCheck (p1, p2,Empty) (c,b,wc,bc)) && -- que al moverse no quede en jaque
                                         isPossible poss p2 && -- el movimiento válido de la pieza (en cuando a dirección)
                                         freeWay p2 poss b -- que no haya nada en el camino

-- Función auxiliar, verifica que no se intente tomar el rey rival
notKing :: Square -> Bool
notKing x = x /= WK && x /= BK
