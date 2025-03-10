import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

revNumToInt :: String -> Int
revNumToInt n = read (reverse n) :: Int

splitInts :: String -> [Int]
splitInts str = reverse $ helper [] [] str where
    helper acc currNumberRev [] = (revNumToInt currNumberRev) : acc
    helper acc currNumberRev (' ':s)
        = helper ((revNumToInt currNumberRev) : acc) "" s
    helper acc currNumberRev (n:s) = helper acc (n:currNumberRev) s


-- We store reversed char codes in map
parseCodes :: [String] -> Map String Char
parseCodes strs = helper (M.empty :: Map String Char) strs where
    helper accMap [] = accMap
    helper accMap (str@(ch:':':' ':remains):strs)
        = helper (M.insert (reverse remains) ch accMap) strs

readCodes :: Int -> IO [String]
readCodes k = helper [] 0 k where
    helper :: [String] -> Int -> Int -> IO [String]
    helper acc i lim
        | i == lim = return acc
        | otherwise = do
            l <- getLine
            helper (l:acc) (i+1) lim

decodeString :: String -> Map String Char -> String
decodeString str codesMap = helper "" "" str codesMap where
    helper :: String -> String -> String -> Map String Char -> String
    -- We lookup using reversed char codes
    helper chars currAccCode "" codesMap = case M.lookup currAccCode codesMap of
        (Just res) -> res:chars
        Nothing -> undefined
    helper chars currAccCode str@(ch:remains) codesMap = case M.lookup currAccCode codesMap of
        (Just res) -> helper (res:chars) "" str codesMap
        Nothing -> helper chars (ch:currAccCode) remains codesMap

main :: IO ()
main = do
    s <- getLine
    let [k, l] = splitInts s
    codesR <- readCodes k
    let codesMap = parseCodes codesR
    encodedStr <- getLine
--    putStrLn $ show k
--    putStrLn $ show l
--    putStrLn $ show codesMap
--    putStrLn $ show encodedStr
    putStrLn $ reverse $ decodeString encodedStr codesMap
