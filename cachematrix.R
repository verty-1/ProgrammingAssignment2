## These functions calculate and cache the inverse of a square matrix
## so it does not need to be recalculated each time the inverse matrix
## is needed. The input matrix "x" must be invertible.  If the "x" is 
## not invertible, an error is produced by the solve() function.

## The inverse will be recalculated if either:
##   1) The inverse matrix is NULL or not present, or 
##   2) The input matrix "x" differs from "previous" calculation
## Otherwise, the inverted matrix in cache will be returned

## These functions are run together, type:
## >cacheSolve(makeCacheMatrix(x)), where "x" is the predefined square 
## input matrix. 
## The inverse matrix result is assigned to "m" in global environment

## The order of execution is cacheSolve() first, which then may call
## makeCacheMatrix() various times.

## This first function makeCacheMatrix() creates a list of four 
## functions. It does not calculate the inverse. 

makeCacheMatrix <- function(x = numeric()) {
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x  ## get function returns x
        setminv <- function(minv) m <<- minv
        getminv <- function() m
        #       message("I run thru every time")
        list(set = set, get = get,
             setminv = setminv,
             getminv = getminv)
}
## This second function cacheSolve() initializes and calculates the 
## inverse matrix "m", or reports the value that was cached
## previously to the global environment. The matrix "previous" is
## also put in the global environment to check for changes to input 
## matrix and recalculate if necessary.

cacheSolve <- function(x, ...) {
        if(!exists("m")) {m <<- NULL} ## Initialize in global env.
        m <- x$getminv() 
        if(!exists("previous")) {previous <<- NULL} ## Initialize
        data <- x$get()
        if(!is.null(m) & identical(data, previous)) {
                message("getting cached inverse matrix")
                return(m)
        }
        ## Calculate and return matrix that is the inverse of "x"
        message("calculated inverse matrix")
        m <- solve(data)
        previous <<- data
        x$setminv(m)
        m
}