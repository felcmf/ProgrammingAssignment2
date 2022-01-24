## This function creates a special "matrix" object that can cache its inverse.
## The function returns a list containing a function to
## 1. Set the matrix
## 2. Get the matrix
## 3. Set the inverse
## 4. Get the inverse

makeCacheMatrix <- function (x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve will retrieve the inverse from 
## the cache. Otherwise, it will return the inverse and store the inverse in 
## the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
