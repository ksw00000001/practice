-- N個の整数aiと正の整数Wが与えられる。前者の部分和で後者を構成できるか
-- input : 1 2 4 6 6
--         13
-- output: Yes

checkConfigurability :: [Integer] -> Integer -> Bool
checkConfigurability [] 0 = True
checkConfigurability [] _ = False
checkConfigurability (x:xs) n
   | n < 0     = False
   | otherwise = (checkConfigurability xs (n-x)) || (checkConfigurability xs n)

main = do
   listOfAi <-fmap (map (read::String ->Integer)) $ fmap words getLine
   w <- fmap (read :: String -> Integer) getLine
   if (checkConfigurability listOfAi w)
   then do
      putStrLn "Yes"
   else do
      putStrLn "No"

