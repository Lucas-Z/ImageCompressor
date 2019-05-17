module Lib
    ( parser,
      usage,
      isNumber,
      first,
      second,
      third,
      verif_converg,
      replace,
      add_tuples,
      div_tuples
    ) where

import Kmeans
import System.Random
import Text.Printf
import System.Environment
import System.Console.GetOpt
import System.Exit
import Text.Read

first :: (a, b, c) -> a
first (a,_,_) = a

second :: (a, b, c) -> b
second (_,b,_) = b

third :: (a, b, c) -> c
third (_,_,c) = c

last1 :: (a, (b, c, d)) -> (b,c,d)
last1 (_, (b,c,d)) = (b,c,d)

first1 :: (a, (b, c, d)) -> a
first1 (a, (_,_,_)) = a

isNumber :: String -> Bool
isNumber str =
    case (reads str) :: [(Double, String)] of
    [(_, "")] -> True
    _         -> False

replace :: String -> String -> Char -> Char -> String
replace [] str a b  = str
replace (x:xs) str a b
        | x == a    = replace xs (str ++ [b]) a b
        | otherwise = replace xs (str ++ [x]) a b

printPoints :: [(Int, (Float, Float, Float))] -> Int -> Int -> [String] -> IO ()
printPoints tab j i points = do
        if (j == (length tab)) then
            return ()
        else if  first1 (tab!!j) == i then do
            putStrLn (points!!j)
            printPoints tab (j+1) i points
        else
            printPoints tab (j+1) i points


printOutput :: [(Float, Float, Float)] -> Int -> [(Int, (Float, Float, Float))] -> [String] -> IO ()
printOutput tab i points str_points = do
        if (i < length tab) then do
            putStrLn "--"
            printf "(%.2f," (first (tab!!i))
            printf "%.2f," (second (tab!!i))
            printf "%.2f)\n" (third (tab!!i))
            putStrLn "-"
            printPoints points 0 i str_points
            printOutput tab (i+1) points str_points
        else
            return ()

parsFile :: Float -> Int -> String -> IO()
parsFile c k path = do
    content <- readFile(path)
    let points = lines (content)
    let tab = map words (lines(content))
    let coord = selectData tab []
    convertToInt c k coord [] points

selectData :: [[String]] -> [String] -> [String]
selectData [] data_list = data_list
selectData (x:xs) data_list =  selectData xs (data_list ++ [x!!1])

getPoint :: String -> [String]
getPoint str = do
    let clean   = (replace str "" ',' ' ')
    let clean1  = (replace clean "" '(' ' ')
    let clean2  = (replace clean1 "" ')' ' ')
    let final   = words clean2
    final

random_nb :: Int -> IO Int
random_nb len = randomRIO (0, len)

gen_k :: Int -> [(Float, Float, Float)] -> Int -> [Int] -> IO [Int]
gen_k k data_list i rand =
        if (i < k) then do
            it <- random_nb ((length data_list)-1)
            gen_k k data_list (i+1) (rand ++ [it])
        else
            return rand

assignValue :: [Int] -> [(Float, Float, Float)] -> [(Float, Float, Float)] -> [(Float, Float, Float)]
assignValue [] idata_list k_tab = k_tab
assignValue (x:xs) idata_list k_tab = assignValue xs idata_list (k_tab ++ [(idata_list!!x)])

verif_converg :: [(Float, Float, Float)] -> [(Float, Float, Float)] -> Int -> Float -> Bool
verif_converg k_tab [] i c = False
verif_converg k_tab k_prev i c
                | (i == length k_tab) = True
                | distance_p_to_p (k_tab!!i) (k_prev!!i) > c = False
                | otherwise = verif_converg k_tab k_prev (i+1) c

add_tuples :: (Float, Float, Float) -> (Float, Float, Float) -> (Float, Float, Float)
add_tuples (a, b, c) (d, e, f) = (a+d,b+e,c+f)

div_tuples :: (Float, Float, Float) -> Float -> (Float, Float, Float)
div_tuples (a,b,c) 0 = (a,b,c)
div_tuples (a, b, c) dividende = (a/dividende,b/dividende,c/dividende)

calc_avg :: [(Float, Float, Float)] -> (Float, Float, Float) -> Float -> [(Float, Float, Float)]
calc_avg k_tab average nb_avg = do
            k_tab ++ [(div_tuples average nb_avg)]

mean :: [(Int, (Float, Float, Float))] -> [(Float, Float, Float)] -> Int -> Int  -> (Float, Float, Float) -> Float -> [(Float, Float, Float)]
mean matrice k_tab i j average nb_avg = do
            if (j == length k_tab) then
                k_tab
            else if (i < (length matrice)) then
                if (j == (first1 (matrice!!i))) then
                    mean matrice k_tab (i+1) j (add_tuples average (last1 (matrice!!i))) (nb_avg+1)
                else
                    mean matrice k_tab (i+1) j average nb_avg
            else do
                let (x:xs) = (calc_avg k_tab average nb_avg)
                mean matrice xs 0 (j+1) (0,0,0) 0

kmeans :: [(Float, Float, Float)] -> [(Float, Float, Float)] -> [(Float, Float, Float)] -> Float -> [(Int, (Float, Float, Float))]
kmeans data_list k_tab k_prev c =
        if ( (verif_converg k_tab k_prev 0 c) == True) then
            near_points data_list k_tab 0 [] []
        else do
            let prev = k_tab
            kmeans data_list (mean (near_points data_list k_tab 0 [] []) k_tab 0 0 (0,0,0) 0) prev c

convertToInt :: Float -> Int -> [String] -> [(Float, Float, Float)] -> [String] -> IO ()
convertToInt c k [] idata_list str_points = do
        it <- gen_k k idata_list 0 []
        let final_map = (kmeans idata_list (assignValue it idata_list []) [] c)
        -- let mean_tab = mean final_map (assignValue it idata_list []) 0 0 (0,0,0) 0
        -- print (mean_tab)
        printOutput (mean final_map (assignValue it idata_list []) 0 0 (0,0,0) 0) 0 final_map str_points
        -- print final_map
convertToInt c k (x:xs) idata_list str_points = do
    let points = getPoint x
    convertToInt c k xs (idata_list ++ [(read (points!!0) :: Float,
        read (points!!1) :: Float, read (points!!2) :: Float)]) str_points

usage :: IO ()
usage = do
    putStrLn "USAGE: ./imageCompressor n e IN"
    putStrLn ""
    putStrLn "\tn\tnumber of colors in the final image"
    putStrLn "\te\tconvergence limit"
    putStrLn "\tIN\tpath to the file containing the colors of the pixels"
    exitWith (ExitFailure(84))

parser::[String] -> IO ()
parser args
        | (length args /= 3)            = usage
        | isNumber (args!!0) == False   = usage
        | otherwise                     = parsFile (read (args!!1) :: Float) (read (args!!0) :: Int) (args!!2)