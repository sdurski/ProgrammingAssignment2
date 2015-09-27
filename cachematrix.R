## These functions provide a special list object to hold a matrix,
## and 'cache' the inverse of the matrix.
## a call to cachesolve on this object, will calculate the inverse if it hasn't 
## yet been or if the matrix has been changed since it was last calculated,
## otherwise the matrix inverse is retrieved from this object.

## makeCacheMatrix is used to 
##   set the matrix values
##   retrieve (get) the matrix values
##   set the inverse of the matrix
##   retrieve (get) the inverse of the matrix (if previously calculated)

makeCacheMatrix <- function(x = matrix()) {
        x_inv <- NULL
        set <- function(y) {
                x <<- y
                x_inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv) x_inv <<- inv
        getinv <- function() x_inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## A call to cacheSolve, checks to see if the inverse of 
## the matrix has been calculated since the last time the
## values of the matrix were set.  If so, the inverse is 
## retrieved, otherwise 'solve' is used to calculate the
## inverse.  Additional arguments may be passed through to
## 'solve' if desired.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x_inv <- x$getinv()
        if(!is.null(x_inv)) {
                message("getting cached data")
                return(x_inv)
        }
        data <- x$get()
        x_inv <- solve(data, ...)
        x$setinv(x_inv)
        x_inv
}
