module Solution (solve)
where
import           Board (Board)
import qualified Board
import           Creek

solve :: Creek -> Board
solve (Creek size conditions) = Board.empty size
