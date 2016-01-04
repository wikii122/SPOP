module Board 
  ( Field
  , Board
  ) where 

data Field = White | Black | Unknown

type Board = [[Field]]
