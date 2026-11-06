


names :: [String] -> (String , String)
names [] = ("", "")
names (x:xs) = (x, last xs) 

firstAndLast :: [String] -> String
firstAndLast [] = ""
firstAndLast (x:xs) = x ++ last xs



join :: [[Int]] -> [Int]
join [] = []
join (x:h) = x ++ join h 



func4 :: [String] -> String
func4 [] = ""
func4 (x:xs) = x ++ "." ++ last xs
