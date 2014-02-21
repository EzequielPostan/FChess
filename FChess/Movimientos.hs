module Movimientos where
import Data.Char
import Estructuras

-- Transforma una caracter previamente analizado en un color
toColor :: Char -> Color
toColor 'B' = White
toColor 'N' = Black
tocolor _ = Grey

--Transforma una cadena previamente analizada en una posición del tablero
toPos :: String -> Pos
toPos [c,f] = (8*(digitToInt f - 1) + g c)
              where g :: Char -> Int
                    g 'a' = 0
                    g 'b' = 1
                    g 'c' = 2
                    g 'd' = 3
                    g 'e' = 4
                    g 'f' = 5
                    g 'g' = 6
                    g 'h' = 7

-- Transforma una cadena previamente analizada en un movimiento
toMove :: String -> Move
toMove [c1,f1,c2,f2] = (toPos [c1,f1], toPos [c2,f2] , Empty)
toMove [c1,f1,c2,f2,p] = (toPos [c1,f1] ,toPos [c2,f2] , h p)
                          where h 'd' = WQ
                                h 't' = WR
                                h 'a' = WB
                                h 'c' = WN


-- getSquare: Dada una posición y un tablero devuelve el contenido de la casilla correspondiente
getSquare :: Pos -> Board -> Square
getSquare p b = b !! p

-- setSquare coloca una pieza en una casilla de un tablero dado
setSquare :: Square -> Pos -> Board -> Board
setSquare s p b = let (u,v) = splitAt p b in u ++ s:if v == [] then [] else tail v

-- delSquare setea en vacía una casilla dada de un tablero
delSquare :: Pos -> Board -> Board
delSquare = setSquare Empty

-- updateBoard actualiza el tablero, no analiza la validez de las jugadas
updateBoard :: Move -> Board -> Board
updateBoard (pos1 , pos2 , Empty) b = let p = getSquare pos1 b in delSquare pos1 $ setSquare p pos2 b
updateBoard (pos1 , pos2 , cor) b = let p = getSquare pos1 b
                                        c = color p
                                    in case c of
                                        Black -> delSquare pos1 $ setSquare (opPiece cor) pos2 b
                                        _ -> delSquare pos1 $ setSquare cor pos2 b

-- Dada una pieza blanca retorna la equivalente en el bando opuesto. Esta función se usa sólo para coronación
opPiece :: Square -> Square
opPiece x = case x of
             WQ -> BQ
             WR -> BR
             WN -> BN
             WB -> BB
             _ -> error "opPiece: Pieza inválida"
             

-- color nos da el color de la pieza en una casilla
color :: Square -> Color
color s = case s of
           Empty -> Grey
           WP -> White
           WN -> White
           WB -> White
           WR -> White
           WQ -> White
           WK -> White
           _ -> Black

-- Dado un color, nos da el opuesto
opColor :: Color -> Color
opColor = \x -> case x of
                 White -> Black
                 Black -> White
                 _ -> Grey

-- Encapsula lo que hace “move'” en la mónada IO
move :: Env -> Move -> IO Env
move env mov = return $ move' env mov

-- Actualiza el tablero y el estado de los enroques. También cambia el color del turno
move' :: Env -> Move -> Env
move' (White,b,wc,bc) mov = let (p1,p2,cor) = mov
                                b' = updateBoard mov b
                                n = castleHandler White p1
                                wc' = updateCastle n wc
                                ck = updateBoard (toMove "h1f1") b' --enroque corto
                                cq = updateBoard (toMove "a1d1") b' --enroque largo
                            in case n of -- se mueve el rey
                                3 -> if p2-p1 == 2 then (Black,ck,wc',bc) else if p2-p1 == -2 then (Black,cq,wc',bc) else (Black,b',wc',bc)
                                _ -> (Black , b' , wc' , bc)
move' (Black,b,wc,bc) mov = let (p1,p2,cor) = mov
                                b' = updateBoard mov b
                                n = castleHandler Black p1
                                bc' = updateCastle n bc
                                ck = updateBoard (toMove "h8f8") b' --enroque corto
                                cq = updateBoard (toMove "a8d8") b' --enroque largo
                            in case n of -- se mueve el rey
                                3 -> if p2-p1 == 2 then (White,ck,wc,bc') else if p2-p1 == -2 then (White,cq,wc,bc') else (White,b',wc,bc')
                                _ -> (White , b' , wc , bc')


-- Funci¿ón auxiliar para manejar los enroques
castleHandler :: Color -> Pos -> Int
castleHandler White n = case n of
                         0 -> 2 -- mueve torre dama
                         4 -> 3 -- mueve el rey
                         7 -> 1 -- mueve torre rey
                         _ -> 0 -- no involucra rey ni torres
castleHandler Black n = case n of
                         56 -> 2 -- mueve torre dama
                         60 -> 3 -- mueve el rey
                         63 -> 1 -- mueve torre rey
                         _ -> 0 -- no involucra rey ni torres

-- Actualiza el estado de los enroques. Toma el handler del último movimiento generado por castleHandler,el estado actual y devuelve el resultado
updateCastle :: Int -> Int -> Int
updateCastle 0 n = n
updateCastle n 0 = n
updateCastle _ 3 = 3
updateCastle 3 _ = 3
updateCastle 1 2 = 3
updateCastle 2 2 = 2
updateCastle 2 1 = 3
updateCastle 1 1 = 1

