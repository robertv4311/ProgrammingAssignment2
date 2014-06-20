## These functions can be used to calculate the inverse
## of a matrix, and to cache the inverse so that it 
## doesn't need to be recalculated each time it is needed.
## To use them, first store makeCacheMatrix() to a variable (v 
## in this example). Then use v$set() to set the matrix, and
## cacheSolve(v) to calculate the inverse. If you run 
## cachesolve(v) again before re-setting the matrix, it will 
##  return the value from the cache rather than recalculating it.

## makeCacheMatrix is a list of four functions that, in order,
## set the value of the original matrix, get that value, 
## set the value of the inverse, and get that value. In
## general, the user will call the set function directly, while the
## function below will use the others to return the value
## of the inverse (whether from the cache or by calculating it).

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(invert) inverse <<- invert
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve is the function the user will call to
## return the inverse of a matrix he or she has 
## defined with the "set" function from makeCacheMatrix.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
