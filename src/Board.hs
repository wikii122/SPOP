module Board
  ( Field
  , Board (Board)
  , Quad
  , (!.), (!#)
  , empty
  , combinations
  ) where

import Creek (Location, Size, Creek)

data Field = White | Black | Unknown | Outer
  deriving Eq

type Quad = ((Field, Field), (Field, Field))

data Board = Board Size [[Field]]

-- Constructor for empty board
empty :: Size -> Board
empty size@(x, y) = Board size (replicate y $ replicate x Unknown)

-- Get value from a field
field :: Board -> Location -> Field
field (Board size fs) (x, y) = if (x, y) `inside` size
  then (fs !! (y)) !! (x)
  else Outer
    where
      (x, y) `inside` (xm, ym) =  x >= 0 && x < xm
                               && y >= 0 && y < ym

-- Get four values from crossing
quad :: Board -> Location -> Quad
quad board (x, y) = (
  (field board (x-1, y-1), field board (  x, y-1)),
  (field board (x-1,   y), field board (  x,   y)))

-- Operators for field and quad operations
-- Get field operator
(!.) :: Board -> Location -> Field
board !. loc = field board loc

-- Get quad operator
(!#) :: Board -> Location -> Quad
board !# loc = quad board loc

-- Used to fill Unknown fields of the quad to sum up given
-- number of black fields
combinations :: Quad -> Int -> [Quad]
combinations quad n = let
    matchQuads :: Int -> [Field] -> [[Field]]
    matchQuads n fs = findCombinations n [] fs

    findCombinations :: Int -> [Field] -> [Field] -> [[Field]]
    findCombinations 0 hs []     = [reverse hs]
    findCombinations _ hs []     = []
    findCombinations n hs (f:fs) = case f of
      Unknown | n > 0     -> (findCombinations n (White:hs) fs) ++ (findCombinations (n-1) (Black:hs) fs)
              | n == 0    -> findCombinations n (White:hs) fs
              | otherwise -> []
      Black   | n > 0     -> findCombinations (n-1) (Black:hs) fs
              | otherwise -> []
      other               -> findCombinations n (other:hs) fs
  in
    map fieldsToQuad $ matchQuads n $ quadToFields quad


-- Helper functions
quadToFields :: Quad -> [Field]
quadToFields ((f1, f2), (f3, f4)) = [f1, f2, f3, f4]

fieldsToQuad :: [Field] -> Quad
fieldsToQuad [f1, f2, f3, f4]     =  ((f1, f2), (f3, f4))

-- Pretty printers
instance Show Field where
  show Black = "■"
  show White = "□"
  show Unknown = "?"
  show _ = "!"

instance Show Board where
  show (Board _ fs) = unlines $ map show' fs
    where
      show' :: [Field] -> String
      show' []     = []
      show' (x:xs) = ' ' : (show x) ++ show' xs