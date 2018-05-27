## Cashing the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.
## It will set the value of the matrix, get the value of the matrix, 
## set the value of the inverse, get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
        }
    get <- function() x
    set_inverse <- function(inverse) m <<- inverse
    get_inverse <- function() m
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
    }


## The following function computes the inverse of the matrix returned by makeCacheMatrix function above
## If the inverse has already been calculated (and the matrix has not changed), the cacheSolve function
## shoud retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$get_inverse()
        if(!is.null(m)) {
            message("Getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inverse(m)
        m
}