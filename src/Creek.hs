module Creek 
  ( Creek
  , Size
  , Location
  , Condition
) where 

type Size = (Int, Int)
type Location = (Int, Int)
type Condition = (Location, Int)

data Creek = Creek Size [Condition] 
  deriving (
    Show,
    Read
    )