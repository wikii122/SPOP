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
                                                Nothing    -> findSolution (Just board) loc qs []
                                                something  -> something
findSolution (Just board) loc []     _      = Nothing
findSolution (Just board) loc (q:qs) (c:cs) = case (findSolution newBoard (fst c) combinations cs) of
                                                Nothing    -> findSolution (Just board) loc qs (c:cs)
                                                something  -> something
                                               where
                                                    newBoard     = Board.insertQuad board loc q
                                                    newQuad      = case newBoard of Just board -> board !# (fst c)
                                                    combinations = Board.combinations newQuad num
                                                    num          = snd c
