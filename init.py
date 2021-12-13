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
    if col == -1:
        return elements
    element = grid[row][col]
    elements.append(element)

    if grid[row][col-1] == -1:
        return elements
    return getRowElements(grid, row, (col-1), elements)

def getColElements(grid, row, col, elements):
    if row == -1: 
        return elements
    
    element = grid[row][col]
    elements.append(element)

    if grid[row-1][col] == -1:
        return elements
    return getColElements(grid, (row-1), col, elements)

def hasNumberInCol(grid, row, col, number):
    colElements = getColElements(grid, row, col, [])
    return number in colElements

def hasNumberInRow(grid, row, col, number):
    rowElements = getRowElements(grid, row, col, [])
    return contains(rowElements, number)

def canAssignNumber(grid, row, col, number):
    if cellHasBeenAssigned(grid, row, col):
        return False
    if hasNumberInRow(grid, row, col, number):
        return False
    if hasNumberInCol(grid, row, col, number):
        return False
    if not checkForSequentialLine(grid, row, col, number):
        return False
    
    return True

# method recursively verifies if there is at least one number that can be placed into cell
# if cell is static, returns true for flow purposes
def canAssignAnyNumber(grid, row, col, number):
    size = len(grid)
    if number == (size+1):
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
    v = element
    if element >= 10:
        v = element/10
    elements.append(v)
    return getElementsUntilStatic(line, (index-1), elements)

# this method goes elements by elements checking if
# diff between current element and next element is == 1
# if diff != 1, then it is not sequential

def getStaticNumber(n):
    if n >= 10:
        v = (n/10)
        return v
    else:
        return n
def isSequential(elements, index):
    size = len(elements)
    if (index+1) == size or size == 0:
        return True
    else:
        diff = abs( (getStaticNumber( elements[index] ) - getStaticNumber( elements[(index+1)]) ) )
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

def checkForSequentialRow(grid, row, col, number):
    rowEle   = getRowElements(grid, row, col, [])
    elements = getElementsUntilStatic(rowEle, len(rowEle)-1, [])
    elements.append(number)
    filtered = [ i for i in elements if i > 0 ]
    filtered.sort()
    size = (len(filtered))
    if size == 1:
        return True
    return isSequential(filtered, 0)

def checkForSequentialCol(grid, row, col, number):
    colEle   = getColElements(grid, row, col, [])
    elements = getElementsUntilStatic(colEle, (len(colEle)-1), [])

    
    elements.append(number)

    filtered = []
    for i in range(len(elements)):
        if elements[i] > 0:
            filtered.append(elements[i])
    filtered.sort()
    size == (len(filtered))
    if size == 1:
        return True
    return isSequential(filtered, 0)

# undo current changed made
def failedAssigningNumber(grid, row, col, number):
    grid[row][col] = 0
    return canAssignAnyNumber(grid, row, col, 1) and solveStr8ts(grid, row, col, (number+1))

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
    
def checkForSequentialLine(grid, row, col, number):
    needToCheckRow = col == (size-1) or needsToCheckRow(grid, row, col)
    needToCheckCol = row == (size-1) or needsToCheckCol(grid, row, col)
    if needToCheckRow:
        rowSeq = checkForSequentialRow(grid, row, col, number)
    if needToCheckCol:
        colSeq = checkForSequentialCol(grid, row, col, number)

    if (needToCheckRow and not rowSeq) or (needToCheckCol and not colSeq):
        return False
    return True

def getNextNonStaticCell(grid, row, col, size):
    col = col-1
    if row == -1:
        return []
    if col == -1:
        return getNextNonStaticCell(grid, (row-1), (size), size)
    if grid[row][col] >= 10 or grid[row][col] == -1:
        return getNextNonStaticCell(grid, row, col, size)
    else:
        return [row, col]
# solver method
# basically, it is a recursive depth first search, i.e, backtracking
# although here are used randomic numbers instead of sequential numbers
def solveStr8ts(grid, not_tested, row, col, size):
    if row == -1 or len(not_tested[0][0]) == 0:
        return False
    if row == size and col == 0:
        return True

    if col == size:
        return solveStr8ts(grid, not_tested, (row+1), 0, size)
    if col == -1:
        return solveStr8ts(grid, not_tested, (row-1), (size-1), size)
    if isStatic(grid, row, (col)) or cellHasBeenAssigned(grid, row, col):
        return solveStr8ts(grid, not_tested, row, (col+1), size)
    number = get_not_tested_number(not_tested, row, col)
    previous = grid[row][col]
    
    if number == -2:
        
        noStaticCell = getNextNonStaticCell(grid, row, col, size)
        r = noStaticCell[0]
        c = noStaticCell[1]
        not_tested = populate_not_tested_numbers(not_tested, r, (c+1), size)
        grid[r][c] = 0
        return solveStr8ts(grid, not_tested, r, c, size)
    
    if canAssignNumber(grid, row, col, number):
        grid[row][col] = number
        return solveStr8ts(grid, not_tested, row, (col+1), size)
    else:
        grid[row][col] = previous
        return solveStr8ts(grid, not_tested, row, col, size)

def populate_not_tested_numbers(matrix, row, col, size):
    if row == size and col == 0:
        return matrix

    if col == size:
        return populate_not_tested_numbers(matrix, (row+1), 0, size)
    numbers = create_list_of_not_tested(size, 0, [])
    matrix[row][col] = numbers
    return populate_not_tested_numbers(matrix, row, (col+1), size)

def create_list_of_not_tested(size, index, line):
    if index == size+1:
        return line
    value = (index+1)
    if contains(line, value):
        return create_list_of_not_tested(size, index, line)
    else:
        line.append(value)
        return create_list_of_not_tested(size, (index+1), line)
    

def create_columns(size, col, line):
    if col == size:
        return line
    numbers = create_list_of_not_tested(size, 0, [])
    line.append(numbers)
    return create_columns(size, (col+1), line)

def create_matrix(matrix, size, row):
    if row == size:
        return matrix
    line = create_columns(size, 0, [])
    matrix.append(line)
    return create_matrix(matrix, size, (row+1))

def get_not_tested_number(matrix, row, col):
    line = matrix[row]
    cell = line[col]
    if len(cell) == 0:
        return -2
    return cell.pop(0)

if __name__ == "__main__":
    grid = [[-1,  0,  0, -1],
            [-1,  0,  0,  0],
            [-1,  10, 0,  0],
            [ 10,  0,  0,  0]]

    size = len(grid)

    matrix = create_matrix([], size, 0)
    
    if not solveStr8ts(grid, matrix, 0, 0, size):
        print("BOOOOH YAHH")
        print("could not solve")
    else:
        print(grid)