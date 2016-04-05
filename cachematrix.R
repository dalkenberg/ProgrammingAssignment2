## Coursera Programming in R, Programming Assignment 2
## These functions allow the user to take the inverse of a matrix and
## store the result in a cached variable.  On subsequent runs in the same 
## session, the resulting matrix will be returned immediately from the cache 
## rather than having to be calculated again.

## Test case:
## c <- rbind(c(1,3), c(3,1))  ## create sample matrix
## solve(c)                    ## view the inverse
## m <- makeCacheMatrix(c)     ## get vector of functions
## cacheSolve(m)               ## get inverse (and cache it)
## cacheSolve(m)               ## get inverse again (this time from cache)


## Creates and returns a vector consisting of four functions that:
## - get and store the matrix parameter to be solved 
## - get and store the inverse matrix after it has been passed to solve
##   from another function

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
  
    get <- function() x
    setsolve <- function(matrix) m <<- matrix
    getsolve <- function() m
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Takes the list that is output from makeCacheMatrix() as input.
## Attempts to load from cache.  If not present, calculates the inverse
## and stores the result in cache, then returns it

cacheSolve <- function(x, ...) {

    m <- x$getsolve()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }

    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
