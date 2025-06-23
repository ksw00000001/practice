import Data.List (sort, nub, intercalate, find)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromJust)

gcd' :: Int -> Int -> Int
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

lcm' :: Int -> Int -> Int
lcm' a b = (a * b) `div` (gcd' a b)

lcmList :: [Int] -> Int
lcmList [] = 1
lcmList [x] = x
lcmList (x:xs) = lcm' x (lcmList xs)

parseInput :: String -> Maybe [Int]
parseInput s =
    let numsStr = words s
        parsedNums = map reads numsStr
        allParsed = all (\r -> case r of [(x, "")] -> True; _ -> False) parsedNums
    in
        if allParsed
            then Just $ map (\r -> fst (head r)) parsedNums
            else Nothing


applyOperation :: [Int] -> [Int] -> Int -> Maybe [Int]
applyOperation v a_constraints job_idx =
    if job_idx < 0 || job_idx >= length v
        then Nothing 
        else
            let
                new_v_list =
                    [
                        if i == job_idx
                            then a_constraints !! i
                            else val - 1
                        | (i, val) <- zip [0..] v
                    ]
            in
                if all (> 0) new_v_list
                    then Just new_v_list
                    else Nothing

type PathResult = ([Int], [Int])

findLoop :: [Int] -> [Int] -> Int -> [Int] -> [Int] -> Map.Map [Int] [Int] -> Maybe PathResult
findLoop initial_v a_constraints max_steps current_v current_path visited_states
    | Map.member current_v visited_states =
        let
            prev_path = visited_states Map.! current_v
            loop_entry_idx = length prev_path
            loop_path = drop loop_entry_idx current_path
        in
            Just (prev_path, loop_path)

    | length current_path > max_steps = Nothing 
    | otherwise =
        let
            new_visited_states = Map.insert current_v current_path visited_states

            tryNextJob :: Int -> Maybe PathResult
            tryNextJob i
                | i >= length a_constraints = Nothing 
                | otherwise =
                    case applyOperation current_v a_constraints i of
                        Just next_v ->
                            case findLoop initial_v a_constraints max_steps next_v (current_path ++ [i + 1]) new_visited_states of
                                Just result -> Just result 
                                Nothing -> tryNextJob (i + 1) 
                        Nothing -> tryNextJob (i + 1) 
        in
            tryNextJob 0 

main :: IO ()
main = do
    inputStr <- getLine
    case parseInput inputStr of
        Just a_constraints -> do
            if null a_constraints then
                putStrLn "NO"
            else do
                let initial_v = map (\x -> x) a_constraints 
                let p = 2 * lcmList a_constraints 

                let result = findLoop initial_v a_constraints p initial_v [] Map.empty

                case result of
                    Just (pre_loop_path, loop_path) -> do
                        if not (null pre_loop_path) then
                            putStrLn $ intercalate " " (map show pre_loop_path)
                        else
                            putStrLn "" 

                        putStrLn $ intercalate " " (map show loop_path)
                    Nothing -> putStrLn "NO"
        Nothing ->
            putStrLn "Invalid input format."
