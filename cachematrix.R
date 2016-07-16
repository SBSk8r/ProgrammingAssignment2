## This script contains 2 functions which will cache the inverse of a square 
## invertible matrix rather than computing it repeatedly.

## The makeCacheMatrix function creates a special "matrix" object that can cache 
## the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL 
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInv <- function(solve) i <<- solve
        getInv <- function() i
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## The cacheSolve function computes the inverse of the "matrix" which was returned by 
## the makeCacheMatrix function above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getInv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        if(class(try(solve(data),silent=T)) != "matrix") {
                message("Matrix cannot be inverted")
                return(i)
        }
        i <- solve(data,...)
        x$setInv(i)
        i
}