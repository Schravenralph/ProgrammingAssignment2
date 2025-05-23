## These functions are used to cache the inverse of a matrix.
## Caching the inverse avoids repeated computation and improves efficiency,
## especially when dealing with large matrices or repeated inverse calculations.

## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of functions to set and get the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        # This function sets the matrix to a new value and resets the cached inverse.
        # '<<-' assigns the value in the parent environment so the matrix and its inverse
        # are updated outside the local function scope.
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated and the matrix has not changed,
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
