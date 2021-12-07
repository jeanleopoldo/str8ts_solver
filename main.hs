module Main where

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

addElementToCell :: [[Int]] -> Int -> Int -> Int -> [[Int]]
addElementToCell grid row col number =
    let line = grid !! row
    in
        let newLine = take col line ++ number : drop col line
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
    if line !! index == -1 || index == -1 then
        elements
    else
        let el = line !! index
        in
            let j = elements ++ [el]
            in
                getElementsUntilStatic line (index-1) j

-- this method goes elements by elements checking if
-- diff between current element and next element is == 1
-- if diff != 1, then it is not sequential
isSequential :: [Int] -> Int -> Bool
isSequential [] _ = True
isSequential elements index =
    index == length elements +1 ||
    (let diff = abs (elements !! index) - (elements !! index+1)
    in
        diff == 1 || isSequential elements (index+1))

-- TODO
checkForSequentialRow :: [[Int]] -> Int -> Int -> Int -> Bool
checkForSequentialRow grid row col number =
    let rowElements = getRowElements grid row col []
    in
        let elements = getElementsUntilStatic rowElements (length rowElements-1) []
        in
            let filtered = filter (>0) elements
            in
                length filtered == 1 || isSequential filtered number

checkForSequentialCol:: [[Int]] -> Int -> Int -> Int -> Bool
checkForSequentialCol grid row col number =
    let colElements = getColElements grid row col []
    in
        let elements = getElementsUntilStatic colElements (length colElements-1) []
        in
            let filtered = filter (>0) elements
            in
                length filtered==1 || isSequential filtered number

needsToCheckRow :: [[Int]] -> Int -> Int -> Bool
needsToCheckRow grid row col =
    let size = length grid
    in
        col == size-1 || getElement grid row (col+1) == -1

needsToCheckCol :: [[Int]] -> Int -> Int -> Bool
needsToCheckCol grid row col =
    let size = length grid
    in
        row == size-1 || getElement grid (row+1) col == -1

sequentialRow :: [[Int]] -> Int -> Int -> Int -> Bool
sequentialRow grid row col number = not (needsToCheckRow grid row col) || checkForSequentialRow grid row col number

sequentialCol :: [[Int]] -> Int -> Int -> Int -> Bool
sequentialCol grid row col number = not (needsToCheckCol grid row col) || checkForSequentialCol grid row col number

canAssignNumber :: [[Int]] -> Int -> Int -> Int -> Bool
canAssignNumber grid row col number = not (cellHasBeenAssigned grid row col
                                            || hasNumberInRow grid row col number
                                            || hasNumberInCol grid row col number)
                                            && sequentialRow grid row col number
                                            && sequentialCol grid row col number

-- method recursively verifies if there is at least one number that can be placed into cell
-- if cell is static, returns true for flow purposes
-- three are the condition to assing a number into a cell:
--     1. no number had been assinged to the cell
--     2. the number must not be in the row this cell is in
--     3. the number must not be in the column this cell is in
canAssignAnyNumber :: [[Int]] -> Int -> Int -> Int -> Bool
canAssignAnyNumber grid row col number =
    number /= length grid+1 &&
    (isStatic grid row col
        || not (cellHasBeenAssigned grid row col)
            && not (hasNumberInRow grid row col number)
            && not (hasNumberInCol grid row col number)) || canAssignAnyNumber grid row col (number+1)

failedToAssignNumber :: [[Int]] -> Int -> Int -> Int -> Bool
failedToAssignNumber grid row col number =
    let j = addElementToCell grid row col 0
    in
        canAssignAnyNumber grid row col 1 && solve grid row col (number+1)


solve :: [[Int]] -> Int -> Int-> Int -> Bool
solve grid row col number
  | isStatic grid row col  || cellHasBeenAssigned grid row col = 
  solve grid row (col+1) number
  | canAssignNumber grid row col number =
    let a = addElementToCell grid row col number
    in
        solve grid row (col+1) (number+1)
  | otherwise =
    failedToAssignNumber grid row col number

main = do
    let grid = [[-1,  0,  0, -1],
                [-1,  0,  0,  0],
                [-1,  0,  1,  0],
                [ 1,  0,  0,  0]]
    let s = solve grid 0 0 1
    if s then
        print grid
    else
        print "could not solve"