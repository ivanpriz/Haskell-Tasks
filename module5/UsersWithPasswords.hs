import Control.Monad.Reader

type User = String
type Password = String
type UsersTable = [(User, Password)]

--asks allow us to get the environment and perform actions on it and returns the res
usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = asks ((map fst) . filter (\x -> snd x == "123456"))