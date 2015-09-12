import Data.Char
import Data.Array

data Pos = Pos (Int, Int)
data PieceType = P | N | B | R | Q | K deriving (Show, Read, Eq, Ord)
data Side = White | Black
data Piece = Piece {side :: Side, ptype :: PieceType, pos :: Pos} 
data Board = Board [Piece]

-- instance Show Board where
showBoard (Board ps) = 
    let board = ar//[(getPos piece, icon piece) | piece<-ps] in
    [[board!(y,x)|y<-[1..8]]|x<-[1..8]]
    where 
        ar = listArray ((1,1),(8,8)) $ cycle "."
        getPos (Piece  _ _ (Pos pos)) = pos
        icon (Piece s t _) = 
            case s of 
                White -> fst
                Black -> snd
            $
            case t of
                P -> ('♙','♟')
                N -> ('♘','♞')
                B -> ('♗','♝')
                R -> ('♖','♜')
                Q -> ('♕','♛')
                K -> ('♔','♚')

main = print "lol"

setup = Board  $
    [Piece White P (Pos (x,2))|x<-[1..8]] ++
    [Piece Black P (Pos (x,7))|x<-[1..8]] ++
    zipWith (Piece White) (map (\l->read[l]) "RNBQKBNR") [Pos (i,1)|i<-[1..8]] ++
    zipWith (Piece Black) (map (\l->read[l]) "RNBKQBNR") [Pos (i,8)|i<-[1..8]]
     

--instance Read Pos where
readPos [file,rank] = Pos (ord file - 96, digitToInt rank)

--instance Show Pos where
showPos (Pos (x,y)) = [chr (x+96), intToDigit y]

-- need to add special moves
legal :: Piece -> Pos -> Bool
legal piece@(Piece s t (Pos (oldX,oldY))) pos@(Pos (newX,newY))=
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
swap (Piece s t p) t' = Piece s t' p

-- Possible moves for a knight from pos
wholeBoard = [Pos (x,y) | x<-[1..8], y<-[1..8]]
nmoves pos = map showPos $ filter (legal$Piece White N (readPos pos)) wholeBoard
next' (p:ps) = [p':p:ps|p'<-nmoves p]
next = concatMap next' 
