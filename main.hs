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
    && not (isStatic grid row col)                -- célula não é estática
    && not (cellHasBeenAssigned grid row col)     -- célula não tem nenhum número vindo no input
    && not (hasNumberInRow grid row col number)   -- não tem esse número na linha
    && not (hasNumberInCol grid row col number)   -- não tem esse número na coluna
    && isSequentialRow grid row col               -- testa para ver se, ao fim de uma linha, se forma uma sequência
    && isSequentialCol grid row col               -- testa para ver se, ao fim de uma coluna, se forma uma sequência 


-- refaz os números não testados a partir desta célula
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
            populateNotTestedNumbers matrix row (col+1) size

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
        let c = (col-1)
        in
            if c == -1 then
                getNextNonStaticCell grid (row-1) (size-1) size
            else
                let el = getElement grid row c
                in
                    if el >= 10 || el == -1 then
                        [row, c]
                    else
                        getNextNonStaticCell grid row c size

finishedAllPossibilititiesForCell :: [[Int]] -> [[[Int]]] -> Int -> Int -> Int -> [[Int]]
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
                        solve grid notTested r c size


createListOfNotTestedNumbers :: Int -> Int -> [Int] -> [Int]
createListOfNotTestedNumbers size col line =
    if size == col then
        line
    else
        createListOfNotTestedNumbers size (col+1) (line ++ [col+1])

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
        let line = createColumns [] 0 size
        in
            createMatrixOfNotTested (matrix ++ [line]) (row+1) size

addElementToCell :: [[Int]] -> Int -> Int -> Int -> [[Int]]
addElementToCell grid row col number =
    let line = grid !! row
    in
        let addedCell = take col line ++ number : drop col line
        in
            let newLine = deleteFromIndex (col+1) addedCell
            in
                take row grid ++ newLine : drop row grid



addListToCell :: [[[Int]]] -> Int -> Int -> [Int] -> [[[Int]]]
addListToCell matrix row col numbers =
    let line = matrix !! row
    in
        let cell = line !! col
        in
            let newCell = take col line ++ numbers : drop col line
            in
                let newLine = deleteFromIndex (col+1) newCell
                in
                    let newMatrix = deleteFromIndex row matrix
                    in
                        take row newMatrix ++ newLine : drop row newMatrix

getNotTestedNumber :: [[[Int]]] -> Int -> Int -> Int
getNotTestedNumber matrix row col =
    let line = matrix !! row
    in
        let cell = line !! col
        in
            if null cell then
                -2
            else
                head cell

isDifferent :: Int -> Int -> Bool
isDifferent a b = a/=b

updateNotTested :: [[[Int]]] -> Int -> Int -> [[[Int]]]
updateNotTested notTested row col =
    let line = notTested !! row
    in
        let cell = line !! col
        in
            let element = head cell
            in
                let j = filter (`isDifferent` element) cell
                in
                    addListToCell notTested row col j

solve :: [[Int]] -> [[[Int]]] -> Int -> Int -> Int -> [[Int]]
solve grid notTested row col size
  | (row == -1) || finishedAllPossibilities notTested = []  -- não conseguiu resolver
  | (row == size) && (col == 0) = grid                      -- conseguiu resolver
  | col == size = solve grid notTested (row+1) 0 size       -- chegou ao fim de uma linha
  | col == -1 = solve grid notTested (row-1) (size-1) size  -- chegou ao início de uma linha na volta do backtracking
  | isStatic grid row col || cellHasBeenAssigned grid row col = solve grid notTested row (col+1) size -- não é possível colocar numero nessa célula
  | otherwise =
    let notTestedNumber = getNotTestedNumber notTested row col
    in
        let updatedNotTested = updateNotTested notTested row col                        -- pega um número não testado para essa célula
        in
            let previous = getElement grid row col                                       -- armazena número que estava na célula anteriormente
            in
                if notTestedNumber == -2 then
                    finishedAllPossibilititiesForCell grid updatedNotTested row col size -- todos os números foram testados, volta na árvore
                else
                    if canAssignNumber grid row col notTestedNumber then                 -- testa se pode pôr esse número na célula
                        let add = addElementToCell grid row col notTestedNumber
                        in
                            solve grid updatedNotTested row (col+1) size                 -- coloca número na célula, vai para a próxima
                    else
                        let add = addElementToCell grid row col previous                 -- não conseguiu coloca número na célula, põe número que estava anteriormente
                        in
                            solve grid updatedNotTested row col size                     -- testa célula novamente
main = do
    let grid =  [[-1,  0,  0, -1, -1, -1],
                 [-1,  0,  0,  0, 50,  0],
                 [-1,  0, 10,  0,  0,  0],
                 [40,  0,  0,  0,  0, -1],
                 [ 0, 60, 50,  0,  0, -1],
                 [-1, -1, -1,  0, 10, -1]]

    let h = [[[]]]
    let is = finishedAllPossibilities h
    print is

    let size = length grid

    let notTestedNumbers = createMatrixOfNotTested [] 0 size

    let s = solve grid notTestedNumbers 0 0 size

    if not (null s)  then
        print s
    else
        print "OH NO! I could not solve"
