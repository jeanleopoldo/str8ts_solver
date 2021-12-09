{-# LANGUAGE BlockArguments #-}
module Main where
import Data.List

contains :: [Int] -> Int -> Bool
contains elements element = element `elem` elements

cellHasBeenAssigned :: [[Int]] -> Int -> Int -> Bool
cellHasBeenAssigned grid row col =
    let cell = getElement grid row col
    in
        cell > 0

addElementToCell :: [[Int]] -> Int -> Int -> Int -> [[Int]]
addElementToCell grid row col number =
    let line = grid !! row
    in
        let addedCell = take col line ++ number : drop col line
        in
            let newLine = deleteFromIndex (col+1) addedCell
            in
                take row grid  ++ newLine : drop row grid

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

checkForSequentialCol:: [[Int]] -> Int -> Int -> Bool
checkForSequentialCol grid row col =
    let colElements = getColElements grid row col []
    in
        let elements = getElementsUntilStatic colElements 0 []
        in
            length elements==1 || isSequential (sort elements) 0

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

createListOfNotTestedNumbers :: Int -> Int -> [Int] -> [Int]
createListOfNotTestedNumbers size index line =
    if size == index then
        line
    else
        createListOfNotTestedNumbers size (index+1) line ++ [index]

createColumns :: [[Int]] -> Int -> Int -> [[Int]]
createColumns line col size =
    if col == size then
        line
    else
        let numbers = createListOfNotTestedNumbers size 0 []
        in
            createColumns (line ++ [numbers]) (col+1) size

createMatrixOfNotTested :: [[[Int]]] -> Int -> Int -> [[[Int]]]
createMatrixOfNotTested matrix row size =
    if row == size then
        matrix
    else
        let line = createColumns [] size 0
        in
            createMatrixOfNotTested (matrix ++ [line]) (row+1) size

--  Tem que testar todas as funções abaixo desta
getNotTestedNumber :: [[[Int]]] -> Int -> Int -> Int
getNotTestedNumber matrix row col =
    let line = matrix !! row
    in
        let cell = line !! col
        in
            if null cell then
                -2
            else
                let n = head cell
                in
                    let h = delete n cell
                    in
                        n

populateNotTestedNumbers :: [[[Int]]] -> Int -> Int -> Int -> [[[Int]]]
populateNotTestedNumbers matrix row col size
  | row == size && col == 0 =
    matrix
  | col == size =
    populateNotTestedNumbers matrix (row+1) 0 size
  | otherwise =
    let numbers = createListOfNotTestedNumbers size 0 []
    in
        let l = addListToCell matrix row col numbers
        in
            populateNotTestedNumbers matrix row col size

addListToCell :: [[[Int]]] -> Int -> Int -> [Int] -> [[[Int]]]
addListToCell matrix row col numbers =
    let line = matrix !! row
    in
        let cell = line !! col
        in
            let a = cell : [numbers]
            in
                matrix

finishedAllPossibilities :: [[[Int]]] -> Bool
finishedAllPossibilities matrix =
    let line = head matrix
    in
        let cell = head line
        in
            null cell
getNextNonStaticCell :: [[Int]] -> Int -> Int -> Int -> [Int]
getNextNonStaticCell grid row col size = 
    if row == -1 then
        []
    else 
        let c = col-1
        in
            if col == -1 then
                getNextNonStaticCell grid (row-1) size size
            else
                let el = getElement grid row col
                in
                    if el >= 10 || el == -1 then
                        getNextNonStaticCell grid row col size
                    else
                        [row, col]

finishedAllPossibilititiesForCell :: [[Int]] -> [[[Int]]] -> Int -> Int -> Int -> Bool
finishedAllPossibilititiesForCell grid notTested row col size =
    let nextNoStaticCell = getNextNonStaticCell grid row col size
    in
        let r = head nextNoStaticCell
        in
            let c = nextNoStaticCell !! 1
            in
                let p = populateNotTestedNumbers notTested r (c+1) size
                in
                    let a = addElementToCell grid r c 0
                    in
                        solve grid notTested row col size

solve :: [[Int]] -> [[[Int]]] -> Int -> Int -> Int -> Bool
solve grid notTested row col size
  | (row == -1) || finishedAllPossibilities notTested =
    False
  | row == size && col == 0 =
    True
  | col == size =
    solve grid notTested (row+1) 0 size
  | col == -1 =
    solve grid notTested (row-1) (size-1) size
  | isStatic grid row col || cellHasBeenAssigned grid row col =
    solve grid notTested row (col+1) size
  | otherwise =
    let notTestedNumber = getNotTestedNumber notTested row col
    in
        let previous = getElement grid row col
        in
            if notTestedNumber == -2 then
                finishedAllPossibilititiesForCell grid notTested row col size
            else
                if canAssignNumber grid row col notTestedNumber then
                    let add = addElementToCell grid row col notTestedNumber
                    in
                        solve grid notTested row col size
                else
                    let add = addElementToCell grid row col previous
                    in
                        solve grid notTested row col size

main = do
    let grid =  [[ 0,  0, -1,  0],
                 [ 2,  0,  1,  0],
                 [-1,  0,  0,  -1],
                 [-1,  2,  0,  1]]

    print (createMatrixOfNotTested [] 0 (length grid))
    -- let s = solveGrid grid 0 0 1

    -- if s then
    --     print grid
    -- else
    --     print "OH NO! I could not solve"
