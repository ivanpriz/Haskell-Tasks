data Result = Fail | Success

data SomeData = S | F | K

doSomeWork :: SomeData -> (Result,Int)

processData :: SomeData -> String
processData x =
  case doSomeWork x of
    (Success, _) -> "Success"
    (Fail, errCode) -> "Fail: " ++ show errCode

data Result' = Result' (Result, Int)


instance Show Result' where
    show (Result' (Success, _)) = "Success"
    show (Result' (Fail, code)) = "Fail: " ++ show code

doSomeWork' :: SomeData -> Result'
doSomeWork' = Result' . doSomeWork
