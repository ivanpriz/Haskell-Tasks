helper :: String -> String
helper name = if length name == 1 then name else (take 1 name) ++ "."

