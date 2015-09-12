import Data.Char
import Data.Array

type Pos = (Int, Int) 
data PieceType = P | N | B | R | Q | K deriving (Show, Read, Eq, Ord)
data Side = White | Black deriving (Eq, Show)
data Piece = Piece {side :: Side, ptype :: PieceType, pos :: Pos} deriving Show
type Board = Array Pos Piece 

-- There's now some cponfusion between Board and [Piece]

main = print "lol"

setup = Board  $
    [Piece White P (x,2)|x<-[1..8]] ++
    [Piece Black P (x,7)|x<-[1..8]] ++
    zipWith (Piece White) (map (\l->read[l]) "RNBQKBNR") [(i,1)|i<-[1..8]] ++
    zipWith (Piece Black) (map (\l->read[l]) "RNBKQBNR") [(i,8)|i<-[1..8]]
     
--instance Read Pos where
readPos [file,rank] = (ord file - 96, digitToInt rank)

--instance Show Pos where
showPos ((x,y)) = [chr (x+96), intToDigit y]

-- need to add special moves
legal :: Piece -> Pos -> Bool
legal piece@(Piece s t (oldX,oldY)) pos@(newX,newY)=
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

move :: Board -> Piece -> Pos -> Board
-- move :: 

swap :: Piece->PieceType->Piece
swap (Piece s t p) t' = Piece s t' p

wholeBoard = [(x,y) | x<-[1..8], y<-[1..8]]
possibleMoves piece = filter (legal piece) wholeBoard


blankBoard = listArray ((1,1),(8,8)) $ cycle "."
boardUpdate board update = board//[(getPos piece, icon piece) | piece<-getPieces board]

-- instance Show Board where
showBoard (Board ps) = 

    [[()!(y,x)|y<-[1..8]]|x<-[1..8]]
    where 
        getPos (Piece  _ _ pos) = pos
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


