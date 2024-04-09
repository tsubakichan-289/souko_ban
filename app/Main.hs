module Main (main) where

import Control.Monad
import Control.Monad.IO.Class 

import System.Console.Haskeline
import System.IO

import Data.Maybe (isNothing, fromJust)
import Data.Foldable (foldlM)

data State = State Int Int [[Int]]
data Point = Point Int Int

instance Show Point where 
    show (Point a b) = "(" ++ show a ++ ", " ++ show b ++ ")"

instance Eq Point where 
    (Point a b) == (Point c d) = a == c && b == d

-- カーソルを指定した位置に移動する関数
moveCursor :: Int -> Int -> IO ()
moveCursor x y = putStr $ "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

-- 指定した位置に文字を表示する関数
putCharAt :: Int -> Int -> Char -> IO ()
putCharAt x y c = do
    moveCursor x y
    putChar c

-- 画面をクリアしてカーソルをホームに移動する関数
cls :: IO ()
cls = do
    putStr "\ESC[2J\ESC[H"
    hFlush stdout

hideCursor :: IO ()
hideCursor = putStr "\ESC[?25l"

showCursor :: IO ()
showCursor = putStr "\ESC[?25h"

updateList :: Int -> Int -> Int -> [[Int]] -> [[Int]]
updateList x y newVal = zipWith (curry updateRow) [1 .. ]
    where
        updateRow (rowIdx, row)
            | rowIdx == x = updateCol row
            | otherwise   = row
        updateCol = zipWith (curry updateVal) [1 .. ]
        updateVal (colIdx, val)
            | colIdx == y = newVal
            | otherwise   = val

stage :: Int -> Int -> [[Int]]
stage x y = do
    a <- [1 .. (2 * x + 1)]
    
    let 
        ll = do 
            b <- [1 .. (2 * y + 1)]
            if a == 1 || a == (2 * x + 1) || b == 1 || b == (2 * y + 1)
                then return 1
                else return 0
    return ll

stage2Str :: [[Int]] -> [[Char]]
stage2Str s = do 
    l <- s 
    let
        ll = do 
            n <- l 
            case n of 
                0 -> return ' '
                1 -> return '%'
                5 -> return '#'
                2 -> return '@'
                _ -> return ' '

    return ll

modifyList :: [Int] -> Maybe [Int]
modifyList (2:xs) =
    let (fives, rest) = span (== 5) xs
    in case rest of
        (0:restAfterZero) -> Just $ [0,2] ++ fives ++ restAfterZero  -- 最初の0を消去し、リストの先頭に0を追加
        _                 -> Nothing
modifyList xs = Nothing

extractUntilEnd :: [[Int]] -> Int -> Int -> String -> Maybe [Int]
extractUntilEnd matrix i j direction = case direction of
    "up"    -> modifyList [ matrix !! y !! j | y <- [i, i-1 .. 0] ]
    "down"  -> modifyList [ matrix !! y !! j | y <- [i .. length matrix - 1] ]
    "left"  -> modifyList [ matrix !! i !! x | x <- [j, j-1 .. 0] ]
    "right" -> modifyList [ matrix !! i !! x | x <- [j .. length (matrix !! i) - 1] ]
    _       -> Nothing

overwriteDirection :: [[a]] -> Int -> Int -> String -> [a] -> [[a]]
overwriteDirection matrix i j direction overwriteList =
    case direction of
        "up" -> updateMatrix i j (-1) 0 overwriteList matrix
        "down" -> updateMatrix i j 1 0 overwriteList matrix
        "left" -> updateMatrix i j 0 (-1) overwriteList matrix
        "right" -> updateMatrix i j 0 1 overwriteList matrix
        _ -> matrix

updateMatrix :: Int -> Int -> Int -> Int -> [a] -> [[a]] -> [[a]]
updateMatrix i j di dj overwriteList matrix =
    let (rows, cols) = (length matrix, length (head matrix))
        indexes = takeWhile (\(x, y) -> x >= 0 && x < rows && y >= 0 && y < cols)
                    $ zip (iterate (+ di) i) (iterate (+ dj) j)
        newMatrix = foldl (\acc (val, (x, y)) -> updateElement acc x y val) matrix (zip overwriteList indexes)
    in newMatrix

updateElement :: [[a]] -> Int -> Int -> a -> [[a]]
updateElement matrix x y val =
    [ if i == x then updateRow row y val else row | (row, i) <- zip matrix [0..] ]

updateRow :: [a] -> Int -> a -> [a]
updateRow row y val = [ if j == y then val else elem | (elem, j) <- zip row [0..] ]


updateStage :: [[Int]] -> Int -> Int -> String -> Maybe [[Int]]
updateStage s x y d = case updateLine of 
    Just lll -> Just $ overwriteDirection s x y d lll
    Nothing -> Nothing
    where
        updateLine = extractUntilEnd s x y d

putStage :: Int -> Int -> Maybe [[Int]] -> [Point] -> InputT IO ()
putStage n_x n_y ups g = if isNothing ups
    then liftIO $ return ()
    else liftIO (do 
        let 
            ups' = fromJust ups
            dones = findFives ups' `intersect` g
        cls
        moveCursor 23 1
        print $ findFives ups'
        moveCursor 23 2
        print g 
        moveCursor 23 3
        print dones
        --mapM_ print $ ups'
        mapM_ (\(Point x y) -> putStrRedAt x y "G") g
        putStrMatrixAt 1 1 $ stage2Str ups'
        mapM_ (\(Point x y) -> putStrRedAt x y "#") dones
        when (length dones == length g) $ do 
            cls
            putStrLn "done!!"
        )

putStrAt :: Int -> Int -> Char -> IO ()
putStrAt row col char = do
    -- カーソルを指定された位置に移動
    putStr $ "\ESC[" ++ show row ++ ";" ++ show col ++ "H"
    -- 文字を出力
    putChar char

putCharOrMove :: Int -> Int -> Char -> IO Int
putCharOrMove row col ' ' = return (col + 1)  -- 空白の場合はカーソルだけ右に移動
putCharOrMove row col char = do
    putStrAt row col char
    return (col + 1)  -- 文字を出力した後にカーソルを右に移動

putStrMatrixAt :: Int -> Int -> [[Char]] -> IO ()
putStrMatrixAt _ _ [] = return ()
putStrMatrixAt row col (x:xs) = do
    col' <- foldlM (putCharOrMove row) col x  -- 各文字に対して処理
    putStrMatrixAt (row + 1) col xs  -- 次の行へ

exeSouko :: Int -> Int -> Maybe [[Int]] -> [[Int]] -> (State -> [Point] -> InputT IO ()) -> [Point]-> InputT IO ()
exeSouko n_x n_y ups sta loop g =
    if isNothing ups
        then do
            putStage n_x n_y (Just sta) g
            loop (State n_x n_y sta) g
        else do
            let
                ups' = fromJust ups

            putStage n_x n_y ups g

            loop (State n_x n_y ups') g

findFives :: [[Int]] -> [Point]
findFives matrix = [Point (i + 1) (j + 1) | (row, i) <- zip matrix [0..], (val, j) <- zip row [0..], val == 5]

intersect :: Eq a => [a] -> [a] -> [a]
intersect listA listB = [x | x <- listA, x `elem` listB]

putStrRedAt :: Int -> Int -> String -> IO ()
putStrRedAt row col text = do
    -- カーソルを指定された位置に移動
    putStr $ "\ESC[" ++ show row ++ ";" ++ show col ++ "H"
    -- 赤い文字でテキストを出力
    putStr $ "\ESC[31m" ++ text ++ "\ESC[0m"
    -- カラー設定をリセット

main :: IO ()
main = do
    let 
        s = updateList 5 7 5 $ updateList 10 10 5 $ updateList 5 6 5 $ updateList 5 5 2 $ stage 10 10
        g = [Point 3 3, Point 3 4, Point 3 5]

    cls 
    runInputT defaultSettings (do 
        putStage 4 4 (Just s) g
        loop (State 4 4 s) g)
    where
        loop :: State -> [Point] -> InputT IO ()
        loop (State x y sta) g = do
            liftIO hideCursor
            minput <- getInputChar ""
            case minput of
                Nothing -> return ()
                Just 'q' -> return ()  -- 'q' を入力すると終了
                Just 'w' -> do
                    let 
                        ups = updateStage sta x y "up"
                        isN = isNothing ups
                        (n_x, n_y) = if isN
                            then (x, y)
                            else (x - 1, y)
                    putStage n_x n_y ups g
                    exeSouko n_x n_y ups sta loop g
                Just 'a' -> do
                    let 
                        ups = updateStage sta x y "left"
                        isN = isNothing ups
                        (n_x, n_y) = if isN
                            then (x, y)
                            else (x, y - 1)
                    putStage n_x n_y ups g
                    exeSouko n_x n_y ups sta loop g
                Just 's' -> do
                    let 
                        ups = updateStage sta x y "down"
                        isN = isNothing ups
                        (n_x, n_y) = if isN
                            then (x, y)
                            else (x + 1, y)
                    putStage n_x n_y ups g
                    exeSouko n_x n_y ups sta loop g
                Just 'd' -> do
                    let 
                        ups = updateStage sta x y "right"
                        isN = isNothing ups
                        (n_x, n_y) = if isN
                            then (x, y)
                            else (x, y + 1)
                    exeSouko n_x n_y ups sta loop g
