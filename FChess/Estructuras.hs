 {-# LANGUAGE UnicodeSyntax #-}
module Estructuras where

-- Distinguiremos tres valores de Color
-- White para las blancas
-- Black para las negras
-- Grey se usará para denotar una casilla vacía
data Color = White | Black | Grey deriving Eq

-- Una casilla puede tener una pieza o estar vacía
data Square = Empty
            |WP
            |WN
            |WB
            |WR
            |WQ
            |WK
            |BP
            |BN
            |BB
            |BR
            |BQ
            |BK
 deriving Eq

-- Representación UNICODE de las piezas de ajedrez
instance Show Square where
      show Empty = " -"
      show WP = " \x2659"
      show WN = " \x2658"
      show WB = " \x2657"
      show WR = " \x2656"
      show WQ = " \x2655"
      show WK = " \x2654"
      show BP = " \x265F"
      show BN = " \x265E"
      show BB = " \x265D"
      show BR = " \x265C"
      show BQ = " \x265B"
      show BK = " \x265A"

-- Lo más intuitivo para el tablero parecería ser una lista de listas
-- Sin embargo por eficiencia y simlicidad para el cálculo de movimientos
-- decidí usar una lista común
type Board = [Square]

prettyBoard :: Color -> Env -> String
-- Si el jugador juega con las blancas
prettyBoard White (_,b,_,_) = show' 8 . reverse . split $ b
                               where show' :: Int -> [[Square]] -> String
                                     show' 0 [] = "\n  +-----------------\n    A B C D E F G H \n"
                                     show' n (x:xs) = "\n" ++ show n ++ " |" ++ (concat.map show) x ++ show' (n-1) xs
                                     split [] = []
                                     split xs = let (h,t) = splitAt 8 xs in  h : split t
-- Si el jugador juega con las negras
prettyBoard Black (_,b,_,_) =  (show' 64 . concat . reverse . split $ b) ++ "  +-----------------\n    H G F E D C B A \n"
                               where show' :: Int -> [Square] -> String
                                     show' 0 [] = "\n1 |"
                                     show' n (x:xs) = case mod n 8 of
                                                       0 -> show' (n-1) xs ++ show x ++ "\n" ++ let q = div n 8 in if q == 8 then "" else show (q+1) ++ " |"
                                                       _ -> show' (n-1) xs ++ show x 
                                     split [] = []
                                     split xs = let (h,t) = splitAt 8 xs in  h : split t


-- Para referirnos a una casilla necesitamos un sólo entero
type Pos = Int
type Move = (Pos,Pos,Square)

initPos :: Board
initPos = [WR , WN , WB , WQ , WK , WB , WN , WR,
           WP , WP , WP , WP , WP , WP , WP , WP,
           Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
           Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
           Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
           Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
           BP , BP , BP , BP , BP , BP , BP , BP,
           BR , BN , BB , BQ , BK , BB , BN , BR
          ]


type Env = (Color , Board , Int , Int)
initEnv :: Env
initEnv = (White, initPos, 0, 0)

data GenTree a = Gen a [GenTree a]

{-
--Algunas funciones para pruebas
instance Show a => Show (GenTree a) where
         show = show' 0
                where show' n (Gen g gs) = replicate (3*n) ' '++ show g ++ "\n" ++ concat (map (\x-> show' (n+1) x) gs)
                                 

genHead :: GenTree a -> a
genHead (Gen a _) = a
 
 
genTail :: GenTree a -> [GenTree a]
genTail (Gen _ gs) = gs
-}
