type Size = (Int, Int)
type Loction = (Int, Int)
type Condition = (Location, Int)

data Creek = Creek Size [Conditions] 
  deriving (
    Show,
    Read
    )