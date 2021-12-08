module Main where
import Data.List

contains :: [Int] -> Int -> Bool
contains elements element = element `elem` elements

cellHasBeenAssigned :: [[Int]] -> Int -> Int -> Bool
cellHasBeenAssigned grid row col =
    let cell = getElement grid row col
    in
        cell > 0

isStatic :: [[Int]] -> Int -> Int-> Bool
isStatic grid row col =
    let cell = getElement grid row col
    in
        cell == -1

getElement :: [[Int]] -> Int -> Int -> Int
getElement grid row col =
    let line = grid !! row
    in
        line !! col

deleteFromIndex :: Int -> [a] -> [a]
deleteFromIndex _ []     = []
deleteFromIndex index (a:as)
   | index == 0    = as
   | otherwise = a : deleteFromIndex (index-1) as

addElementToCell :: [[Int]] -> Int -> Int -> Int -> [[Int]]
addElementToCell grid row col number =
    let line = grid !! row
    in
        let addedCell = take col line ++ number : drop col line
        in
            let newLine = deleteFromIndex (col+1) addedCell
            in
                take row grid  ++ newLine : drop row grid

getRowElements :: [[Int]] -> Int -> Int -> [Int]-> [Int]
getRowElements grid row col elements =
    let e = getElement grid row col
    in
        let h = elements ++ [e]
        in
            if col == 0 then
                h
            else
                getRowElements grid row (col-1) h

getColElements :: [[Int]] -> Int -> Int -> [Int] -> [Int]
getColElements grid row col elements =
    let e = getElement grid row col
    in
        let h = elements ++ [e]
        in
            if row == 0 then
                h
            else
                getColElements grid (row-1) col h

hasNumberInRow :: [[Int]] -> Int -> Int -> Int -> Bool
hasNumberInRow grid row col number =
    let elements = getRowElements grid row col []
    in
        not (null elements) && contains elements number

hasNumberInCol :: [[Int]] -> Int -> Int -> Int -> Bool
hasNumberInCol grid row col number =
    let elements = getColElements grid row col []
    in
        not (null elements) && contains elements number

getElementsUntilStatic :: [Int] -> Int -> [Int] -> [Int]
getElementsUntilStatic line index elements =
    let el = line !! index
    in
        if index == length line -1 || line !! index == -1 then
            elements ++ [el]
        else
            getElementsUntilStatic line (index+1) (elements ++ [el])

-- this method goes elements by elements checking if
-- diff between current element and next element is == 1
-- if diff != 1, then it is not sequential
isSequential :: [Int] -> Int -> Bool
isSequential [] _ = True
isSequential elements index =
    index == length elements-1
    || diff (elements !! index) (elements !! (index+1)) && isSequential elements (index+1)

diff:: Int -> Int -> Bool
diff a b = abs (a-b) == 1



failedToAssignNumber :: [[Int]] -> Int -> Int -> Int -> Bool
failedToAssignNumber grid row col number =
    let j = addElementToCell grid row col 0
    in
        canAssignAnyNumber grid row col 1 && solve grid row col (number+1)

-- solver method
-- basically, it is a recursive depth first search, i.e, backtracking
-- although here are used randomic numbers instead of sequential numbers

-- condition to check if it is end of table
-- if true: all cell has been filled
solve :: [[Int]] -> Int -> Int-> Int -> Bool
solve grid row col number =
    let size = length grid
    in
        row == size-1 && col == size || (
        if col == size then
            solve grid (row+1) 0 number
        else
            if isStatic grid row col || cellHasBeenAssigned grid row col then
                solve grid row (col+1) (number+1)
            else
                if canAssignNumber grid row col number then
                    let h = addElementToCell grid row col
                    in
                        solve grid row (col+1) (number+1)
                else
                    let h = addElementToCell grid row col 0
                    in
                        False

checkForSequentialCol :: [[Int]] -> Int -> Int -> Bool
checkForSequentialCol grid row col =
    let colElements = getColElements grid row col []
    in
        let elements = getElementsUntilStatic colElements 0 []
        in
            length elements==1 || isSequential (sort elements) 0

-- TODO
checkForSequentialRow :: [[Int]] -> Int -> Int ->  Bool
checkForSequentialRow grid row col =
    -- get row elements
    let rowElements = getRowElements grid row col []
    in
        -- get list of numbers until it finds initial edge of the table
        let elements = getElementsUntilStatic rowElements 0 []
        in
            -- get elements until the next number were put by the algorithm
            length elements == 1 || isSequential (sort elements) 0

needsToCheckRow :: [[Int]] -> Int -> Int -> Bool
needsToCheckRow grid row col =
    col >= 0 &&
    (length grid -1 == col || getElement grid row (col-1) == -1)

needsToCheckCol :: [[Int]] -> Int -> Int -> Bool
needsToCheckCol grid row col =
    row >= 0 && (length grid -1 == row || getElement grid (row-1) col == -1)

isSequentialRow :: [[Int]] -> Int -> Int -> Bool
isSequentialRow grid row col = needsToCheckRow grid row col && checkForSequentialRow grid row col

isSequentialCol :: [[Int]] -> Int -> Int -> Bool
isSequentialCol grid row col = needsToCheckCol grid row col && checkForSequentialCol grid row col

canAssignNumber :: [[Int]] -> Int -> Int -> Int -> Bool
canAssignNumber grid row col number =
    number < length grid + 1
    && not (isStatic grid row col)
    && not (cellHasBeenAssigned grid row col)
    && not (hasNumberInRow grid row col number)
    && not (hasNumberInCol grid row col number)

main = do
    let grid =  [[ 1,  1, -1,  3],
                 [ 2,  3,  1,  4],
                 [-1,  4,  3,  2],
                 [-1,  2,  2,  1]]

    let re = solve

    if re then
        print "BOOH YA"
        print grid
    else
        print "OH NO! I could not solve"
        print grid