## Implementation of caching the inverse of
## a matrix:

## A special matrix type that can hold its
## inverse is created by makeCacheMatrix
## function. Function returns a list object
## containin 4 functions: set, get, setinverse
## getinverse. Formal argument is the matrix to
## be inverted. It is assumed to be a square
## matrix. Initally inverse of the matrix x
## is null and it is set to null again if
## matrix x is reassigned by set function.
makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setinverse <- function(inverse) {
        inv <<- inverse
    }
    
    getinverse <- function() {
        inv
    }
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## A special solve function, cacheSolve, is
## implemented to get the inverse of a matrix.
## Formal argument x is a special list,
## created by makeCacheMatrix function.
## This function calls getinverse function of x
## and if value is not null, cached inverse is
## returned. If cached value is null, solve function
## of R is called to obtain inverse and the value
## is stored by calling setinverse fuction of x
## and returned.
cacheSolve <- function(x, ...) {
    
    inv <- x$getinverse()
    
    if (!is.null(inv)) {
        return(inv)
    }
    
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    
    inv
}
