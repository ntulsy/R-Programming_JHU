## makeCacheMatrix creates a special matrix which is a list that helps
## to access get/set function and get/setinverse function.
##
## cacheSolve is a function which will compute the inverse of matrix
## generated from makeCacheMatrix. If the inverse has been computed 
## and there is no change of underlying matrix, use the cache.
## Otherwise, compute the inverse and store it in cache.

## It returns a list with get/set and get/setinverse access to 
## an underlying matrix.
## Parameters:
## x is the actual matrix
## m is the cache of inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        # assign to parent environment variable
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    # Can't use matrix() here as it does not take in ... as parameter
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'.
## It get inverse matrix by firsly looking at cache. 
## If cache is not available, compute inverse and store it.
## Parameters:
## x must be an object generated from makeCacheMatrix

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    # If cache exists
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # If cache does not exist
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
