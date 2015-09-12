import Data.Char

data Pos = Pos (Int, Int)
data PieceType = P | N | B | R | Q | K
data Side = White | Black
data Piece = Piece (Side, PieceType, Pos) -- record syntax?
data Board = Board [Piece]

main = print "lol"

setup = Board [Piece (White, P,Pos (1,1))]

--instance Read Pos where
readPos [file,rank] = Pos (ord file - 96, digitToInt rank)

--instance Show Pos where
showPos (Pos (x,y)) = [chr (x+96), intToDigit y]

-- need to add special moves
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

-- Possible moves for a knight from pos
wholeBoard = [Pos (x,y) | x<-[1..8], y<-[1..8]]
nmoves pos = map showPos $ filter (legal$Piece(White,N,readPos pos)) wholeBoard
next' (p:ps) = [p':p:ps|p'<-nmoves p]
next = concatMap next' 
