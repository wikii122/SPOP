module Solution (solve)
where
import           Board (Board, (!.),  (!#))
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
                                                Just board' -> passable board' 
findSolution (Just board) loc []     _      = Nothing
findSolution (Just board) loc (q:qs) (c:cs) = case (findSolution newBoard (fst c) combinations cs) of
                                                Nothing     -> findSolution (Just board) loc qs (c:cs)
                                                something   -> something
                                               where
                                                    newBoard     = Board.insertQuad board loc q
                                                    newQuad      = case newBoard of Just board -> board !# (fst c)
                                                    combinations = Board.combinations newQuad num
                                                    num          = snd c

passable :: Board -> Maybe Board
passable (Board.Board x fields) | countWhite boardResult == countWhite flagResult = ( Just (Board.Board x boardResult))
                                | otherwise = Nothing  where 
                                   [boardResult, flagResult] = fillBoard fields

findWhite :: [[Board.Field]]  -> Prelude.Maybe Location
findWhite [] =  Nothing
findWhite fields = let
  findWhite' :: [[Board.Field]] -> Int -> Int  -> Prelude.Maybe Location
  findWhite' [] _ _ =  Nothing
  findWhite' ((x:xs):ys) a b    | x == Board.White =  Just (a,b)
                              | otherwise  = findWhite' (xs:ys) (a+1) b
  findWhite' ([]:xs) a b = findWhite' (xs) 0 (b + 1)
  in 
  findWhite' fields 0 0


getElement :: [[Board.Field]] -> Location -> Maybe Board.Field
getElement [] _ = Nothing
getElement ((x:xs):ys) (0,0) = Just x
getElement ([]:xs) (a,b) | a > 0 = Nothing
             | otherwise = getElement (xs) (a-1, 0)
getElement ((x:xs):ys) (a,0)  = getElement (xs:ys) (a-1, 0)
getElement (xs:ys) (a,b) | b < 0 = Nothing
             | otherwise  = getElement ys (a, b-1)

setElement :: [[Board.Field]] -> Maybe Location -> Board.Field -> [[Board.Field]]
setElement xs Nothing _ = xs
setElement [] _ _= []
setElement ((x:xs):ys) (Just (0,0)) z  = ((z:xs):ys)
setElement ([]:xs) (Just (a,b)) z     | a > 0     = (xs)
                        | otherwise = (setElement (xs) (Just (a-1, 0)) z)
setElement ((x:xs):ys) (Just (a,0)) z = ((x:xss):yss) where (xss:yss) = setElement (xs:ys) (Just (a-1, 0)) z 
setElement (xs:ys) (Just (a,b)) z     | b < 0 = (xs:ys)
                        | otherwise  = (xs:setElement ys (Just (a, b-1)) z)

prepareFlagTable :: [[Board.Field]] -> [[Board.Field]]
prepareFlagTable [] = []
prepareFlagTable ([]:xs) = []:prepareFlagTable(xs)
prepareFlagTable ((x:xs):ys) = (Board.Unknown:xss):yss where 
                      (xss:yss) = prepareFlagTable(xs:ys)

copyWhite :: [[Board.Field]]  -> [[[Board.Field]]] 
copyWhite xs = copyWhite' [xs, prepareFlagTable xs ] (findWhite xs)
--[BOARD, FLAGS]
copyWhite' :: [[[Board.Field]]]  -> Maybe Location -> [[[Board.Field]]]
copyWhite' [] _ = []
copyWhite' xs Nothing = xs
copyWhite' [board, flags] (Just (x,y))  | getElement flags (x,y) == (Just Board.White) = [board, flags] 
                    | getElement board (x,y) == (Just Board.White) = copyWhite' (copyWhite' (copyWhite' (copyWhite' [board, setElement flags (Just (x,y)) Board.White ] (Just (x-1, y))) (Just (x, y-1))) (Just (x, y+1))) (Just (x+1, y)) 
                      | getElement board (x,y) == (Just Board.Unknown) = copyWhite' (copyWhite' (copyWhite' (copyWhite' [setElement board (Just (x,y)) Board.White, setElement flags (Just (x,y)) Board.White ] (Just (x-1, y))) (Just (x, y-1))) (Just (x, y+1))) (Just (x+1, y)) 
                      | otherwise = [board, flags] 

fillBlack :: [[Board.Field]] -> [[Board.Field]]
fillBlack [] = []
fillBlack ([]:ys)     = ([]:yss) where (yss) = fillBlack (ys)
fillBlack ((x:xs):ys) | x == Board.Unknown = ((Board.Black:xss):yss) 
            | otherwise = ((x:xss):yss) where (xss:yss) = fillBlack (xs:ys)

fillBoard :: [[Board.Field]] -> [[[Board.Field]]]
fillBoard [] = []
fillBoard xs = [board', flag] where 
      [board, flag] = copyWhite xs 
      board' = fillBlack ( board )


countWhite :: [[Board.Field]] -> Int
countWhite [] = 0
countWhite ([]:xs) = countWhite(xs)
countWhite ((x:xs):ys)  | x == Board.White = (1+ countWhite (xs:ys))
            | otherwise =  countWhite (xs:ys)
