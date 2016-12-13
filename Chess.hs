module Chess where

import Debug.Trace

import Data.Char
import Data.Array
import Data.Maybe
import Data.List
import Data.Ord

import Text.Read

import Control.Monad
import Control.Monad.Loops

type Pos = (Int, Int)
data PieceType = P | N | B | R | Q | K deriving (Show, Read, Eq, Ord, Enum)
data Side = Black | White deriving (Eq, Show)
data Piece = Piece {side :: Side, ptype :: PieceType, pos :: Pos} deriving Show
type Board = Array Pos (Maybe Piece)
type Move = (Piece, Pos)

play = do
  putStrLn "Let's play a game"
  iterateM_ (humanMove White >=> aiMove Black) startBoard

humanMove s b = do
  printBoard b
  putStrLn $ show s ++ " to move."
  putStrLn "Enter move, or hit return for choices"
  moveString <- getLine
  if moveString == ""
    then choiceMove s b
    else parsedMove s b moveString

parsedMove s b moveString =
  let parse = parseMove' moveString in
    case parse of
      Nothing -> humanMove s b
      Just m ->
        return $ uncurry (doMove b) $ specificMove s b m


specificMove :: Side -> Board -> (PieceType,Pos) -> Move
specificMove s b move@(p,(f,r)) =
  let candidates = filter (\x-> side x==s && ptype x==p) $ getPieces b
      piece = head $ filter (\x-> legal startBoard (x,(1,3))) candidates --TODO unsafe
  in
    (piece,(f,r))
{-

parsedMove s b = do
  putStrLn "No parse, try again."
  humanMove s b
parsedMove s b (Just move) =
  if legal move then return move
  else
    putStrLn "Illegal move, try again."
    humanMove s b
-}
choiceMove s b =  do
  let moves = getMoves b s
--mapM_ putStrLn [show n++". "++ agnMove move | move <- moves]
  mapM_ putStrLn [show n++". "++ agnMove (moves !! n) |n<-[0..length moves-1]]
  moveNumber <- getLine
  return $ uncurry (doMove b) (moves !! read moveNumber)

aiMove s b = do
  putStrLn $ "\n Computer plays " ++ agnMove bestMove ++ ".\n"
  return $ uncurry (doMove b) bestMove
  where
    bestMove = bestMoveBy evaluateBoard s b

setup :: [Piece]
setup =
  [Piece White P (x,2)|x<-[1..8]] ++
  [Piece Black P (x,7)|x<-[1..8]] ++
  zipWith (Piece White) (map (\l->read[l]) "RNBKQBNR") [(i,1)|i<-[1..8]] ++
  zipWith (Piece Black) (map (\l->read[l]) "RNBQKBNR") [(i,8)|i<-[1..8]]

startBoard = boardUpdate blankBoard setup

-- fromEnum 'a' = 97
readPos [file,rank] = (readFile' file, digitToInt rank)
readFile' file =   -- avoid conflict with Prelude.readFile
    if file `elem` ['a'..'h'] then Just (ord file - 96) else Nothing
showPos (x,y) = [chr (x+96), intToDigit y]

agnMove :: Move -> String
agnMove (Piece _ t _, pos) = (if t==P then "" else show t) ++ showPos pos

parseMove' [pieceType,file,rank] = do -- I am master of the maybe monad
  p <- readMaybe $ return pieceType
  f <- readFile' file
  r <- readMaybe $ return rank
  return (p,(f,r))
parseMove' [{-pawn,-} file, rank] =
  parseMove' ['P',file,rank]
parseMove _ = trace "couldn't parse move" Nothing

--TODO need to add special moves
legal :: Board -> Move -> Bool
legal board (piece@(Piece s t (oldX,oldY)), newPos@(newX,newY))
  =
  inBounds newX && inBounds newY -- don't go out of bounds
  &&
    dX+dY/=0 -- no non-moves!
  &&
  case t of -- gotta move according to the rules
              --trace ("dX,f1,hE: " ++ concatMap show [dX==0, forward 1,not hitEnemy ]++ show piece ++ show newPos ) False ||
    P -> (dX==0           && forward 1 && not hitEnemy)  || -- normal move -- TODO allow promotion
         (dX==1           && forward 1 &&     hitEnemy)  || -- kill
         (oldY`elem`[2,7] && forward 2 && not hitEnemy)     -- first move --TODO allow en passant
    N -> (dX, dY) `elem`[(1,2),(2,1)]
    B -> dX==dY
    R -> dX==0 || dY==0
    Q -> any (\t'-> legal board (swap piece t', newPos)) [B,R]
    K -> not (dX>1 || dY>1)       --TODO castling
  &&
  (null inbetweens -- can't pass through other pieces
    || (((pos firstHit == newPos) --unless killing
      || t == N) && -- or a knight
      hitEnemy)) --and you have to kill the other team
  where
    pm = case s of Black->(-); White->(+) -- Black moves down the board and white up
    inBounds x =  x>0 && x<9
    dX = abs $ oldX-newX
    dY = abs $ oldY-newY
    inbetweens = catMaybes $ tail [board!(x,y)  -- all the pieces inbetween
      | x <- between oldX newX, y <- between oldY newY]
    between a b = case compare a b of
      LT -> [a..b]
      GT -> reverse [b..a]
      EQ -> [a]
    firstHit = head inbetweens
    hitEnemy =  case board!newPos of
      Nothing -> False
      Just victim -> side victim /= s
    forward n =
      oldY`pm`n==newY

doMove :: Board -> Piece -> Pos -> Board
doMove board piece to =
  if legal board (piece, to) then
  board//[
    (pos piece,Nothing),
    (to, Just $ piece {pos=to})]
  else error "Illegal Move Bro!"

getPieces :: Board -> [Piece]
getPieces = catMaybes.elems

swap :: Piece->PieceType->Piece
swap (Piece s t p) t' = Piece s t' p

wholeBoard = [(x,y) | x<-[1..8], y<-[1..8]]

--TODO this is the slowest and most naive way to do this
possibleMoves board piece = filter (\pos-> legal board (piece,pos)) wholeBoard

blankBoard :: Board
blankBoard = listArray ((1,1),(8,8)) $ repeat Nothing  -- Johnny Tighlips

boardUpdate :: Board -> [Piece] -> Board
boardUpdate board update = board//[(pos piece, Just piece) | piece<-update]

-- instance Show Board where
showBoard :: Board -> [String]
showBoard ps =
  [[case ps!(y,x) of
    Just p -> icon p
    Nothing -> '.'
  | y<-[1..8]]| x<-[1..8]]

printBoard b = do
  putStrLn $ ' ':['a'..'h']
  mapM_ putStrLn . reverse $ zipWith (:) ['1'..'8'] . showBoard $ b
  -- ^ print the lines in reverse order because arrays aren't chess boards

icon (Piece s t _) =
  case s of
    Black -> fst
    White -> snd
  $
  case t of
    P -> ('♙','♟')
    N -> ('♘','♞')
    B -> ('♗','♝')
    R -> ('♖','♜')
    Q -> ('♕','♛')
    K -> ('♔','♚')

justMove b =
  map (\p ->doMove b p (head $ possibleMoves b p))
    $ getPieces b

getMoves b s =
  concatMap (\x-> zip (repeat x) $ possibleMoves b x) $ getSide b s

getSide b s = filter (\x-> side x == s) $ getPieces b

nextBoards b s = map (uncurry $ doMove b) $ getMoves b s

-- the hard part

type Evaluator = Side -> Board -> Int

bestMoveBy :: Evaluator -> Side -> Board -> Move
bestMoveBy eval s b = maximumBy (comparing $ eval s . uncurry (doMove b)) $ getMoves b s

evaluateBoard :: Evaluator
evaluateBoard = iterate evalPredictive evalMaterial !! 2 -- any more than three takes forever

evalSimple s b = fromIntegral . length $ getSide b s

evalMaterial s b =
  sum .
  map (\(Piece s1 t _)->sign s s1 * value t) $
  getPieces b
  where
    sign me it = if it==me then 1 else (-1)

evalPredictive e s b = e s $ uncurry (doMove b) $ bestMoveBy e s b

-- Misc

value :: PieceType -> Int
value t =   case t of
  P -> 1
  N -> 3
  B -> 3
  R -> 5
  Q -> 9
  K -> 1000
