import Data.Char
import Data.Array
import Data.Maybe

type Pos = (Int, Int) 
data PieceType = P | N | B | R | Q | K deriving (Show, Read, Eq, Ord)
data Side = White | Black deriving (Eq, Show)
data Piece = Piece {side :: Side, ptype :: PieceType, pos :: Pos} deriving Show
type Board = Array Pos (Maybe Piece) 

-- There's now some cponfusion between Board and [Piece]

main = print "lol"

setup :: [Piece]
setup = 
    [Piece White P (x,2)|x<-[1..8]] ++
    [Piece Black P (x,7)|x<-[1..8]] ++
    zipWith (Piece White) (map (\l->read[l]) "RNBKQBNR") [(i,1)|i<-[1..8]] ++
    zipWith (Piece Black) (map (\l->read[l]) "RNBQKBNR") [(i,8)|i<-[1..8]]
     
--instance Read Pos where
readPos [file,rank] = (ord file - 96, digitToInt rank)

--instance Show Pos where
showPos ((x,y)) = [chr (x+96), intToDigit y]

-- need to add special moves
legal :: Board -> Piece -> Pos -> Bool
legal board piece@(Piece s t (oldX,oldY)) newPos@(newX,newY)=
    inBounds newX && inBounds newY &&  -- don't go out of bounds
    (null inbetweens -- can't pass through other pieces 
        || (((pos firstHit == newPos) --unless killing
            || t == N) && -- or a knight
            ((side.fromJust $ (board!newPos)) /= s))) && --and you have to kill the other team
    dX+dY/=0 && -- no non-moves!
    case t of
        P -> dX==0 && oldY`pm`1==newY  
        N -> (dX, dY) `elem`[(1,2),(2,1)]
        B -> dX==dY
        R -> dX==0 || dY==0
        Q -> any (\t'-> legal board (swap piece t') newPos) [B,R]
        K -> not (dX>1 || dY>1)
    where 
        pm = case s of White->(+); Black->(-)
        inBounds x =  x>0 && x<9
        dX = abs $ oldX-newX
        dY = abs $ oldY-newY
        inbetweens = catMaybes $ tail [board!(x,y)
                    |x<-between oldX newX,y<-between oldY newY]
        between a b = case compare a b of
                        LT -> [a..b]
                        GT -> reverse [b..a]
                        EQ -> [a]
        firstHit = head inbetweens



move :: Board -> Piece -> Pos -> Board
move board piece to = 
    if legal board piece to then
    board//[
        (pos piece,Nothing),
        (to, Just $ piece {pos=to})]
    else error "Illegal Move Bro!"

getPieces :: Board -> [Piece]
getPieces = catMaybes.elems

swap :: Piece->PieceType->Piece
swap (Piece s t p) t' = Piece s t' p

wholeBoard = [(x,y) | x<-[1..8], y<-[1..8]]
possibleMoves board piece = filter (legal board piece) wholeBoard

blankBoard :: Board
blankBoard = listArray ((1,1),(8,8)) $ repeat Nothing

boardUpdate :: Board -> [Piece] -> Board
boardUpdate board update = board//[(pos piece, Just piece) | piece<-update]

-- instance Show Board where
showBoard :: Board -> [String]
showBoard ps =
    [[case ps!(y,x) of 
        Just p -> icon p
        Nothing -> '.'
    | y<-[1..8]]| x<-[1..8]]
printBoard = mapM_ putStrLn . showBoard
        
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

justMove b = let p = head$getPieces b in move b p (head $ possibleMoves b p)
