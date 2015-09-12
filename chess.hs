import Data.Char

data Pos = Pos (Int, Int)
data PieceType = P | N | B | R | Q | K
data Side = White | Black
data Piece = Piece (Side, PieceType, Pos)
data Board = Board [Piece]

main = print "lol"

setup = Board [Piece (White, P,Pos (1,1))]

--instance Read Pos where
readPos [file,rank] = Pos (ord file - 96, digitToInt rank)


legal :: Piece -> Pos -> Bool
legal piece@(Piece (s, t, Pos (oldX,oldY))) pos@(Pos (newX,newY))=
    inBounds newX && inBounds newY &&
    dX+dY/=0 && -- no non-moves!
    case t of
        P -> dX==0 && oldY`pm`1==newY  
        N -> (dX, dY) `elem`[(1,2),(2,1)]
        B -> dX==dY
        R -> dX==0 || dY==0
        Q -> any (\t'-> legal (swap piece t') pos) [B,R]
        K -> not (dX>1 || dY>1)
    where 
        pm = case s of White->(+); Black->(-)
        inBounds x =  x>0 && x<9
        dX = abs $ oldX-newX
        dY = abs $ oldY-newY

swap :: Piece->PieceType->Piece
swap (Piece (s,t,p)) t' = Piece(s,t',p)
