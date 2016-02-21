## makeCacheMatrix and cacheSolve functions are used in conjunction
## when you need to find the inverse of a matrix and cache the result
## so that you can recall the result without calculating it again.
##
## Usage:
##
## First create a special 'matrix' from usual matrix using makeCacheMatrix function:
##
##      cached_inverse <- makeCacheMatrix(my_matrix)
##
## Then get the inverse matrix by calling cacheSolve function providing the special 
## matrix variable as argument:
##
##      cacheSolve(cached_inverse)
##
## This will output the inverse of original matrix. If you make the same call again,
## it will not make the calculation, but return the cached result of first call.


## Creates special 'matrix' for caching the result

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    set_inverse <- function(inv) inverse <<- inv
    get_inverse <- function() inverse
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
    
}


## Given a special 'matrix' created by makeCacheMatrix function, 
## returns the inverse matrix which is calculated by solve function.
## If called again with same argument, it doesn't call solve functions
## but returns previously cached result

cacheSolve <- function(x, ...) {
    inv <- x$get_inverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$set_inverse(inv)
    inv
}
