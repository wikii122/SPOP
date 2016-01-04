module Board 
  ( Field
  , Board
  ) where 

data Field = White | Black | Unknown
  deriving (Show, Eq)

type Board = [[Field]]
