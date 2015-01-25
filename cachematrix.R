## The following functions are for assignment #2 of the John Hopkins R Programming course on Coursera.
## The functions provide a means to construct a matrix and get and cache the inverse, to demonstrate
## the scoping of variables.

## Returns a list containing a set of functions to operate on the matrix that it represents.
## Notably for the assignment, the inverse of the matrix can be set and retrieved.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y)
    {
        x <<- y
        inverse <<- NULL
    }
    get <- function()
    {
        x
    }
    setInverse <- function(i)
    {
        inverse <<- i
    }
    getInverse <- function()
    {
        inverse
    }
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returns the inverse of the matrix that x, a list created by makeCacheMatrix(), represents.
## The inverse is returned from the cached value if it exists.  Otherwise, the inverse is computed,
## cached, then returned.
## Assumes that the given matrix is invertible.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if (!is.null(inverse))
    {
        return(inverse)
    }
    inverse <- solve(x$get(), ...)
    x$setInverse(inverse)
    return(inverse)
}
