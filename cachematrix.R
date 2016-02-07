## Library to cache a matrix using a set of two functions of which one will store 
## the matrix to determine whether it has already been cached and one function to inverse the set matrix.
## When the matrix has not been cached or the matrix has been changed, the matrix will be inversed and stored in the cache.

## Created by Stefan Leever @ 2016-02-07

## makeCacheMatrix(x)
## This function sets the initial value for the cached matrix (cm), defines the getter and setter functions and returns a list of functions and values.
## Arguments:
## x: a square matrix.
## Return:
## returns a list with the setter, getter, currently inserted matrix and the original matrix.

makeCacheMatrix <- function (x = matrix()) {
    cm <- NULL
    om <<- x
    set <- function(x) {cm <<- x}
    get <- function() cm
    list(set = set, get = get, currentMatrix = x, originalMatrix = om)
}


## cacheSolve(x)
## This function takes the returned list from makeCacheMatrix and inverses the matrix when it's not cached yet.
## If either the matrix has not been cached yet OR the matrix is not the same, it will inverse the matrix and store it in the cache if it's a square matrix.
## Arguments:
## x: makeCacheMatrix list.
## Return:
## returns the inverted matrix.


cacheSolve <- function(x) {
    matrix <- x$get()
    if (!is.null(matrix)) {
        if (identical(x$originalMatrix, x$currentMatrix)) {
            return(matrix)
        }
    }
    matrix <- solve(x$currentMatrix)
    x$set(matrix)
    return (matrix)
}

# Use the code below to execute the above functions and inverse the matrix.
x <- makeCacheMatrix(matrix(1:4, ncol=2, nrow=2))
y <- cacheSolve(x)
