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
    colElements = getColElements(grid, col)
    return number in colElements

def hasNumberInRow(grid, row, col, number):
    rowElements = getRowElements(grid, row, col, [])
    return contains(rowElements, number)

def canAssignNumber(grid, row, col, number):
    if isStatic(grid, row, col):
        return False
    if cellHasBeenAssigned(grid, row, col):
        return False
    if hasNumberInRow(grid, row, col, number):
        return False
    if hasNumberInCol(grid, row, col, number):
        return False
    return True

def solveStr8ts(grid, row, col, n, tried):
    size = len(grid)
    if row == size-1 and col == size:
        print(grid)
        return True
    
    if col == size:
        col = 0
        row = row+1

def solve(grid, row, col):
    solveStr8ts(grid, row, col)

def getMatrix(grid, row, col, matrix):
    
    
    size = len(grid)
    if row == (size-1) and col == size:
        return matrix
    if col == size:
        col = 0
        row = row + 1
    

if __name__ == "__main__":
    grid = [[-1,  0,  0, -1],
            [-1,  0,  0,  0],
            [-1,  0,  1,  0],
            [ 4,  0,  0,  0]]
    matrix = getMatrix(grid, 0, 0, [])
    solve(grid, 0, 0, matrix)