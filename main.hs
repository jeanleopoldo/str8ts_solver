{-# LANGUAGE BlockArguments #-}
module Main where

contains :: [Int] -> Int -> Bool
contains elements element =
    -- must check if it works
    let b = filter (== element) elements
    in
        null b

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

getRowElements :: [[Int]] -> Int -> Int -> [Int] -> [Int]
getRowElements grid row col elements = elements
        -- let i = getElement grid row col
        -- in
        --     elements ++ [i]
        --     let next = getElement grid row (col-1)
        --     in
        --         if next == -1 then
        --             elements
        --         else
        --             getRowElements grid row (col-1) elements

hasNumberInRow :: [[Int]] -> Int -> Int -> Int -> Bool
hasNumberInRow grid row col number = False

hasNumberInCol :: [[Int]] -> Int -> Int -> Int -> Bool
hasNumberInCol grid row col number = False

checkForSequentialLine :: [[Int]] -> Int -> Int -> Int -> Bool
checkForSequentialLine grid row col number = True

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
            && not (hasNumberInCol grid row col number)))

failedToAssignNumber :: [[Int]] -> Int -> Int -> Int -> Bool
failedToAssignNumber grid row col number = True
    -- addElementToCell grid row col 0
    -- True

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
    print ""