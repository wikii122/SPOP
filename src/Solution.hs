module Solution (solve)
where
import           Board (Board (Board), (!.),  (!#))
import qualified Board
import           Creek

solve :: Creek -> Either String Board
solve (Creek size []    ) = Right $ Board.whites size
solve (Creek size (c:cs)) = case (findSolution (Just board) loc combinations cs) of
  Just b -> Right b
  Nothing -> Left "There is no solution"
  where
    board        = Board.empty size
    combinations = Board.combinations quad num
    quad         = board !# loc
    loc          = fst c
    num          = snd c


findSolution :: Maybe Board -> Location -> [Board.Quad] -> [Condition] -> Maybe Board
findSolution  Nothing     _    _      _     = Nothing
findSolution (Just board) loc (q:qs) []     = case (Board.insertQuad board loc q) of
                                                Nothing     -> findSolution (Just board) loc qs []
                                                Just board' -> passable $ Board.rotate board'
findSolution (Just board) loc []     _      = Nothing
findSolution (Just board) loc (q:qs) (c:cs) = case (findSolution newBoard (fst c) combinations cs) of
                                                Nothing     -> findSolution (Just board) loc qs (c:cs)
                                                something   -> something
                                               where
                                                    newBoard     = Board.insertQuad board loc q
                                                    newQuad      = case newBoard of Just board -> board !# (fst c)
                                                    combinations = Board.combinations newQuad num
                                                    num          = snd c

--Check is board acceptable- are all white fields connected.
passable :: Board -> Maybe Board
passable (Board x fields) | countWhite (Board size1 boardResult) == 
                            countWhite (Board size2 flagResult) = ( Just(Board x boardResult))
                          | otherwise                           = Nothing
                              where
                            [Just (Board size1 boardResult), Just (Board size2 flagResult)] = fillBoard (Board x fields)

--Find location of first white board
findWhite :: Board  ->  Location
findWhite (Board size fields) =  findElement (Board size fields) Board.White

--Find location of first field with given type
findElement :: Board -> Board.Field -> Location
findElement  (Board size fields) x = findElement'  (Board size fields) x 0 0 

findElement' :: Board-> Board.Field -> Int -> Int  -> Location
findElement' (Board size [])          _ _ _              = (-1,-1)
findElement' (Board size ((x:xs):ys)) z a b | x == z     = (a,b)
                                            | otherwise  = findElement' (Board size (xs:ys)) z (a+1) b
findElement' (Board  size ([]:xs))    z a b              = findElement' (Board  size (xs)) z 0 (b + 1)

--1. find white field
--2. change all undefined neighbours of field to White
--3. repeat for all White neighbours.
copyWhite :: Board  -> [Maybe Board]
copyWhite (Board size xs)  | whiteX == -1  = copyWhite'[(newBoard), (newFlagsTable)] unknownField
                           | otherwise     = copyWhite' [Just(Board size xs), 
                                                      Just(Board.empty size) ] (whiteX, whiteY) where
                              (whiteX, whiteY) = findWhite (Board size xs) 
                              unknownField     = findElement (Board size xs)  Board.Unknown
                              newBoard         = Board.insertField (Board size xs) unknownField Board.White
                              newFlagsTable    = Just(Board.empty size)

--[BOARD, FLAGS]
copyWhite' :: [Maybe Board]  -> Location -> [Maybe Board]
copyWhite' (Nothing:_)                 _        = [Nothing, Nothing]
copyWhite' []                          _        = []
copyWhite' [Just(board), Just(flags)]  (x,y)  | Board.field (flags) (x,y) == Board.White 
                                                      = [Just(board), Just(flags)]
                                              | Board.field board (x,y) == Board.White 
                                                            = copyWhite' (
                                                                 copyWhite' (
                                                                   copyWhite' (
                                                                     copyWhite' [Just(board), 
                                                                                 Board.insertField flags (x,y) Board.White ] (x-1, y)) 
                                                                   (x, y-1))
                                                                 (x, y+1)) 
                                                              (x+1, y)
                                              | Board.field board (x,y) == Board.Unknown 
                                                            = copyWhite' (
                                                                 copyWhite' (
                                                                   copyWhite' (
                                                                     copyWhite' [(Board.insertField board (x,y) Board.White), 
                                                                                 (Board.insertField flags (x,y) Board.White )] (x-1, y)) 
                                                                   (x, y-1))
                                                                (x, y+1))
                                                              (x+1, y)
                                              | otherwise   =  [Just(board), Just(flags)]

--change all undefined fields to Black
fillBlack :: Maybe Board -> Maybe Board
fillBlack Nothing = Nothing
fillBlack (Just(Board size []))            = Just(Board size [])
fillBlack (Just(Board size ([]:ys)))       = Just(Board size ([]:yss)) where 
                                                  Just(Board size (yss)) = fillBlack (Just(Board size (ys)))
fillBlack (Just(Board size ((x:xs):ys)))   | x == Board.Unknown  = Just(Board size (((Board.Black:xss):yss)))
                                           | otherwise           = Just(Board size ((x:xss):yss)) where 
                                                                        Just(Board size (xss:yss)) = fillBlack (Just(Board size (xs:ys)))
--1. Fill fields with white color using copyWhite function
--2. All Undefined fields set to Black
fillBoard :: Board -> [Maybe Board]
fillBoard (Board size []) = []
fillBoard xs              = [board', flag] where
                              [board, flag] = copyWhite xs
                              board'        = fillBlack ( board )

--Count white fields in board
countWhite :: Board -> Int
countWhite (Board  _    [])          = 0
countWhite (Board size ([]:xs))      = countWhite (Board size xs)
countWhite (Board size ((x:xs):ys))  | x == Board.White 
                                                 = (1 + countWhite (Board size (xs:ys)))
                                     | otherwise = countWhite (Board size (xs:ys))
