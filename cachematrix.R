## This file contains two functions, one to initialize matrix values and a list of handling functions,
## and the second to call the initialized functions and perform the inverse calculation.

## This first function (makeCacheMatrix), takes a matrix as an input and initializes a list
## populated by functions that handle that matrix specifically. These functions in order are:
## - set (to explicitly re-set the cached matrix if desired) 
## - get (to return the currently cached value for the matrix)
## - setinverse (which explicitly caches the argument as the inverse)
## - getinverse (which returns the matrix currently cached as the inverse)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function (cacheSolve) calls the individual functions initialized in the list made by 
## makeCacheMatrix in order to actually return the inverse of the matrix passed as the first argument.
## If data is cached, it will return the cached values and not perform any calculations.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
