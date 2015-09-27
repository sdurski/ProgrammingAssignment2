## These functions provide a special list object to hold a matrix,
## and 'cache' the inverse of the matrix.
## a call to cachesolve on this object, will calculate the inverse if it hasn't 
## yet been or if the matrix has been changed since it was last calculated,
## otherwise the matrix inverse is retrieved from this object.

## makeCacheMatrix is used to 
##   set the matrix values
##   retrieve the matrix values
##   set the inverse of the matrix
##   retrieve the inverse of the matrix (if previously calculated)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## A call to cacheSolve, checks to see if the inverse of 
## the matrix has been calculated since the last time the
## values of the matrix were set.  If so, the inverse is 
## retrieved, otherwise 'solve' is used to calculate the
## inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
