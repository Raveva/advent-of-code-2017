import Data.List.Split
import Data.List
import qualified Data.Set as Set

type Passphrase = String

valid :: Passphrase -> Bool
valid pass = Set.size set == length ws
    where
        ws = words pass
        set = Set.fromList $ map (\w -> sort w) ws

main = do
    input <- readFile "day4/input.txt"

    putStrLn $ show (count (filter (valid) (splitOn "\n" input)))

count :: [a] -> Int
count = foldr (+) 0 . map (\_ -> 1)