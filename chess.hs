import Data.Char

data Pos = Pos (Int, Int)
data PieceType = P | N | B | R | Q | K
data Side = White | Black
data Piece = Piece (Side, PieceType, Pos)
data Board = Board [Piece]

type Move = (Piece,Pos)

main = print "lol"

setup = Board [Piece (White, P,Pos (1,1))]

--instance Read Pos where
readPos [file,rank] = Pos (ord file - 96, digitToInt rank)


legal :: Move -> Bool
legal (Piece (s, a, Pos (oldX,oldY)), Pos (newX,newY)) =
    inBounds newX && inBounds newY &&
    case a of
        P -> oldX==newX && oldY `pm` 1==newY  
        N -> (abs$oldX-newX, abs$oldY-newY) `elem`[(1,2),(2,1)]
    where 
        pm = case s of White->(+); Black->(-)
        inBounds x =  x>0 && x<9 
