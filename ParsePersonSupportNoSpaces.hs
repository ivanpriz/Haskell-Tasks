import Debug.Trace
import Data.Maybe
import Data.Either
import Text.Read

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show


charPresentInString :: Char -> String -> Bool
charPresentInString ch "" = False
charPresentInString ch (s:str) = if s == ch then True else charPresentInString ch str


splitString :: Char -> String -> [String]
splitString delimeter str = case charPresentInString delimeter str of
  True -> reverse $ helper [] delimeter str where
          helper res delimeter "" = res
          helper res delimeter string = let
            chunk = takeWhile (\x -> x /= delimeter) string
            remains = dropWhile (\x -> x == delimeter) $ dropWhile (\x -> x /= delimeter) string
            in helper (chunk:res) delimeter remains
  False -> []


data Field = Field { name :: String, value :: String } deriving Show

strip :: String -> String
strip str = reverse $ dropWhile (\x -> x == ' ') $ reverse $ (dropWhile (\x -> x == ' ') str)

parseField :: String -> Maybe Field
parseField f = case splitString '=' f of
  (name_:value_:[]) -> Just Field {name=strip name_,value=strip value_}
  _ -> Nothing

getFieldByName :: String -> [Maybe Field] -> Maybe Field
getFieldByName fieldName [] = Nothing
getFieldByName fieldName ((Just f):fs) =
  if name f == fieldName
  then Just f
  --    trace ("f=" ++ show f ++ " fieldName=" ++ show fieldName ++ " name f=" ++ show (name f))
  else (getFieldByName fieldName fs)
getFieldByName fieldName (Nothing:fs) = getFieldByName fieldName fs

getGetters :: [String] -> [[Maybe Field] -> Maybe Field]
getGetters names = map helper names where helper x = getFieldByName x

--checkFieldsByName :: [String] -> [Field] -> Either [Maybe Field] IncompleteDataError
getFieldsByNames names fields = map (helper fields) (getGetters names) where helper fields checker = checker fields

parsePersonFields str = getFieldsByNames ["firstName", "lastName", "age"] $ map parseField (splitString '\n' str)


parsePerson :: String -> Either Error Person
parsePerson str = case splitString '\n' str
                  of
                  [] -> Left ParsingError
                  _ -> case any (\x -> if isNothing x then True else False) (map parseField (splitString '\n' str)) of
                       True -> Left ParsingError
                       False -> case any (\x -> if isNothing x then True else False) (parsePersonFields str)
                         of
                         False -> handleAgeError str (readMaybe (value $ fromJust (getFieldByName "age" (parsePersonFields str))) :: Maybe Int)
                           where
                             handleAgeError :: String -> Maybe Int -> Either Error Person
                             handleAgeError str (Just age) = Right (Person {
                                           firstName = value $ fromJust (getFieldByName "firstName" (parsePersonFields str)),
                                           lastName = value $ fromJust (getFieldByName "lastName" (parsePersonFields str)),
                                           age = age
                                          })
                             handleAgeError str Nothing = Left (IncorrectDataError str)
                         True -> Left IncompleteDataError
