{-# LANGUAGE BlockArguments #-}
module Main where

contains :: [Int] -> Int -> Bool
contains elements element = element `notElem` elements

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
    if col == -1 then
        elements
    else
        let e = getElement grid row col
        in
            let h = elements ++ [e]
            in
                let j = getElement grid row (col-1)
                in
                    if j == -1 then
                        h
                    else
                        getRowElements grid row (col-1) h

getColElements :: [[Int]] -> Int -> Int -> [Int] -> [Int]
getColElements grid row col elements =
    if row == -1 then
        elements
    else
        let e = getElement grid row col
        in
            let h = elements ++ [e]
            in
                let j = getElement grid (row-1) col
                in
                    if j == -1 then
                        h
                    else
                        getRowElements grid (row-1) col h

hasNumberInRow :: [[Int]] -> Int -> Int -> Int -> Bool
hasNumberInRow grid row col number =
    let elements = getRowElements grid row col []
    in
        contains elements number

hasNumberInCol :: [[Int]] -> Int -> Int -> Int -> Bool
hasNumberInCol grid row col number =
    let elements = getColElements grid row col []
    in
        contains elements number


    -- if index >= len(line):
    --     index = (len(line)-1)

getElementsUntilStatic :: [Int] -> Int -> [Int] -> [Int]
getElementsUntilStatic line index elements =
    if (line !! index == -1) || (index == -1) then
        elements
    else
        let el = line !! index
        in
            let j = elements ++ [el]
            in
                getElementsUntilStatic line (index-1) j

isSequential :: [Int] -> Int -> Bool
isSequential [] _ = True
isSequential elements index =
    (index == length elements +1) ||
    (let diff = abs (elements !! index) - (elements !! index+1)
    in
        diff == 1 || isSequential elements (index+1))

checkForSequentialLine :: [[Int]] -> Int -> Int -> Int -> Bool
checkForSequentialLine grid row col number =
    let rowElements = getRowElements grid row col []
    in
        let elements = getElementsUntilStatic rowElements (length rowElements-1) []
        in
            let filtered = filter (>0) elements
            in
                (length filtered == 1) || isSequential filtered number

canAssignNumber :: [[Int]] -> Int -> Int -> Int -> Bool
canAssignNumber grid row col number = not (cellHasBeenAssigned grid row col
                                            || hasNumberInRow grid row col number
                                            || hasNumberInCol grid row col number
                                            || not (checkForSequentialLine grid row col number))

canAssignAnyNumber :: [[Int]] -> Int -> Int -> Int -> Bool
canAssignAnyNumber grid row col number =
    number /= (length grid+1) &&
    (isStatic grid row col
        || (not (cellHasBeenAssigned grid row col)
            && not (hasNumberInRow grid row col number)
            && not (hasNumberInCol grid row col number))) || canAssignAnyNumber grid row col (number+1)

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