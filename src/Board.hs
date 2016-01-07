module Board
  ( Field
  , Board
  ) where

import Creek (Location, Size, Creek)

data Field = White | Black | Unknown
  deriving (Show, Eq)

type Quad = ((Field, Field), (Field, Field))

type Board = [[Field]]

-- Constructor for empty board
empty :: Size -> Board
empty (x, y) = replicate y $ replicate x Unknown

-- Get value from a field
field :: Board -> Location -> Field
field board (x, y) = (board !! y) !! x

-- Get four values from crossing
quad :: Board -> Location -> Quad
quad board (x, y) = ((ys1 !! x, ys1 !! (x+1)),
                     (ys2 !! x, ys2 !! (x+1)))
                      where
                        ys1 = board !! y
                        ys2 = board !! (y+1)

-- Operators for field and quad operations
-- Get field operator
(!) :: Board -> Location -> Field
board ! loc = field board loc

-- Get quad operator
(!#) :: Board -> Location -> Quad
board !# loc = quad board loc