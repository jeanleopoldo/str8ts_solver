import random
def isStatic(grid, row, col):
    static = grid[row][col] == -1
    return static

def cellHasBeenAssigned(grid, row, col):
    assigned = grid[row][col] > 0
    return assigned

def contains(elements, number):
    return number in elements

def getRowElements(grid, row, col, elements):
    size = len(grid)
    if col == size:
        return elements
    element = grid[row][col]
    elements.append(element)
    return getRowElements(grid, row, (col+1), elements)

def getColElements(grid, row, col, elements):
    size = len(grid)
    if row == size: 
        return elements
    
    element = grid[row][col]
    elements.append(element)
    return getColElements(grid, (row+1), col, elements)

def hasNumberInCol(grid, row, col, number):
    colElements = getColElements(grid, 0, col, [])
    return number in colElements

def hasNumberInRow(grid, row, col, number):
    rowElements = getRowElements(grid, row, 0, [])
    return contains(rowElements, number)

def canAssignNumber(grid, row, col, number):
    if cellHasBeenAssigned(grid, row, col):
        return False
    if hasNumberInRow(grid, row, col, number):
        return False
    if hasNumberInCol(grid, row, col, number):
        return False
    
    return True

def canAssignAnyNumber(grid, row, col, number):
    size = len(grid)
    if number == size:
        return False
    if isStatic(grid, row, col):
        return True
    numberAssigned = cellHasBeenAssigned(grid, row, col)
    numberInRow    = hasNumberInRow(grid, row, col, number)
    numberInCol    = hasNumberInCol(grid, row, col, number)

    if (not numberAssigned and not numberInRow and not numberInCol):
        return True
    return canAssignAnyNumber(grid, row, col, (number+1))

def checkForSequentialRow(grid, row, col):
    return True

def checkForSequentialCol(grid, row, col):
    return True

def failedAssigningNumber(grid, row, col):
    if row == 3 and col == 0:
        print("s")
    grid[row][col] = 0
    if canAssignAnyNumber(grid, row, col, 1):
        return solveStr8ts(grid, row, col, (random.randint(1, size)))
    
    col = col-1
    if col == 0:
        row = row-1
        col = len(grid)-1
    if row == 0 and col == 0:
        return False
    grid[row][col] = 0
    return solveStr8ts(grid, row, col, (random.randint(1, size)))
def solveStr8ts(grid, row, col, number):
    size = len(grid)
    if row == size-1 and col == size:
        print(grid)
        return True
    
    if col == size:
        col = 0
        row = row+1
    
    if isStatic(grid, row, col):
        return solveStr8ts(grid, row, (col+1), (random.randint(1, size)))
    
    if cellHasBeenAssigned(grid, row, col):
        solveStr8ts(grid, row, (col+1), (random.randint(1, size)))
    
    if canAssignNumber(grid, row, col, number):
        grid[row][col] = number
        number = number+1
        size == len(grid)
        if row == (size-1):
            if checkForSequentialRow(grid, row, 0):
                return solveStr8ts(grid, row, (col+1), random.randint(1, size))
            else:
                failedAssigningNumber(grid, row, col)
        if col == (size-1):
            if(checkForSequentialCol(grid, 0, col)):
                return solveStr8ts(grid, row, (col+1), random.randint(1, size))
            else:
                return failedAssigningNumber(grid, row, col)
        return solveStr8ts(grid, row, (col+1), (random.randint(1, size)))
    else:
        return failedAssigningNumber(grid, row, col)
    
if __name__ == "__main__":
    grid = [[-1,  0,  0, -1],
            [-1,  0,  0,  0],
            [-1,  0,  1,  0],
            [ 4,  0,  0,  0]]

    size = len(grid)

    number = random.randint(1, size)

    if not solveStr8ts(grid, 0, 0, number):
        print("could not solve")