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
    if row == -1: 
        return elements
    
    element = grid[row][col]
    elements.append(element)
    return getColElements(grid, (row-1), col, elements)

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

# method recursively verifies if there is at least one number that can be placed into cell
# if cell is static, returns true for flow purposes
def canAssignAnyNumber(grid, row, col, number):
    size = len(grid)
    if number == size:
        return False
    if isStatic(grid, row, col):
        return True

    # three are the condition to assing a number into a cell:
    # 1. no number had been assinged to the cell
    # 2. the number must not be in the row this cell is in
    # 3. the number must not be in the column this cell is in

    numberAssigned = cellHasBeenAssigned(grid, row, col)
    numberInRow    = hasNumberInRow(grid, row, col, number)
    numberInCol    = hasNumberInCol(grid, row, col, number)

    if (not numberAssigned and not numberInRow and not numberInCol):
        return True
    return canAssignAnyNumber(grid, row, col, (number+1))

def getElementsUntilStatic(line, index, elements):
    if index == -1:
        return elements

    if index >= len(line):
        index = (len(line)-1)
    if line[index] == -1:
        return elements
    element = line[index]
    elements.append(element)
    return getElementsUntilStatic(line, (index-1), elements)

# this method goes elements by elements checking if
# diff between current element and next element is == 1
# if diff != 1, then it is not sequential
def isSequential(elements, index):
    size = len(elements)
    if (index+1) == size or size == 0:
        return True
    else:
        diff = abs((elements[index] - elements[(index+1)]))
        if diff > 1:
            return False
        else:
            return isSequential(elements, (index+1))

# this is the main method, alongside 'checkForSequentialCol'
# that checks is a row has sequential elements
# 1. it gets all elements from the row starting from given column
# until it finds the end of the row or a static cell.
# 2. it sorts elements
# it goes backwards, coming from the end to the begining

def checkForSequentialRow(grid, row, col):
    rowEle   = getRowElements(grid, row, 0, [])
    elements = getElementsUntilStatic(rowEle, col, [])
    elements.sort()
    if size == 0:
        return True
    return isSequential(elements, 0)

def checkForSequentialCol(grid, row, col):
    rowEle   = getRowElements(grid, 0, col, [])
    elements = getElementsUntilStatic(rowEle, row, [])
    size == (len(elements))
    if size == 1:
        return True
    return isSequential(elements, (size-1))

# undo current changed made
def failedAssigningNumber(grid, row, col):
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

def needsToCheckRow(grid, row, col):
    size = len(grid)
    if col == size-1:
        return True
    if grid[row][col+1] == -1:
        return True
    return False

def needsToCheckCol(grid, row, col):
    size = len(grid)
    if row == size-1:
        return True
    if grid[row+1][col] == -1:
        return True
    return False
    
# solver method
# basically, it is a recursive depth first search, i.e, backtracking
# although here are used randomic numbers instead of sequential numbers
def solveStr8ts(grid, row, col, number):
    
    size = len(grid)
    # condition to check if it is end of table
    # if true: all cell has been filled
    if row == size-1 and col == size:
        return True
    
    # condition to go to the next row
    if col == size:
        col = 0
        row = row+1
    
    # condition to check it is a static cell (here is '-1')
    if isStatic(grid, row, col):
        return solveStr8ts(grid, row, (col+1), (random.randint(1, size)))
    
    # condition to check if this cell has been filled already
    # if true, go to next cell
    if cellHasBeenAssigned(grid, row, col):
        solveStr8ts(grid, row, (col+1), (random.randint(1, size)))
    
    # condition to check whether number can be assigned to cell [row][col]
    # if true: assign number
    # if false: discard this branch of computation
    if canAssignNumber(grid, row, col, number):
        grid[row][col] = number
        number = number+1
        size == len(grid)

        # condition to check if it needs to check for sequential elements in row
        if row == (size-1) or needsToCheckRow(grid, row, col):

            # condition to check whether it row has sequential elements
            # if true: move on
            # if false: discard this branch of computation
            if checkForSequentialRow(grid, row, col):
                return solveStr8ts(grid, row, (col+1), random.randint(1, size))
            else:
                failedAssigningNumber(grid, row, col)
        
        # condition to check if it needs to check for sequential elements in column
        if col == (size-1) or needsToCheckCol(grid, row, col):
            # condition to check whether it column has sequential elements
            # if true: move on
            # if false: discard this branch of computation
            if(checkForSequentialCol(grid, row, col)):
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
    else:
        print(grid)