## This function creates a special "matrix" object that can cache its inverse.
## It returns a list with the original matrix as the first object
## and the inverse as the second object.

makeCacheMatrix <- function(x = matrix()) {
    original <<- x
    inverse <<- solve(x) 
    list(original=original, inverse=inverse)
}


## This function computes the inverse of a matrix 'x' 
## If the inverse has already been calculated (and the matrix has not changed) 
## then the cachesolve will retrieve the inverse from the cache.
## Otherwise, it will calculate and return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    if(x==original && !is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    solve(x)
}
