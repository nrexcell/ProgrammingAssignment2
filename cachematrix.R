## Simple package to allow caching of matrix inverses to avoid recalculating each time they are called


## Object which extends matrix functionality by allowing caching of the inverse matrix.
## Cache is cleared if matrix is changed.
makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) xinv <<- solve
    getinv <- function() xinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Returns the inverse of a makeCacheMatrix matrix from cache if it exists.
## Otherwise calculates the inverse and automatically caches the result
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
