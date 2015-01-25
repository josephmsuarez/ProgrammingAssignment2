## cachematrix.R
## Assignment2 - write a pair of functions that cache the inverse of a matrix.
## Joseph M. Suarez
## Submitted 1/25/2015

## makeCacheMatrix - creates a special "matrix" object that can cache its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
    x_inverse <- NULL
    set <- function(y) {
        x <<- y
        x_inverse <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) x_inverse <<-inverse
    getinverse <- function() x_inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve - This function computes the inverse of the special "matrix" 
## returned by the makeCacheMatrix function. If the inverse has already been
## calculated (and the matrix has not changed), then cacheSolve retrieves the
## inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x_inverse <- x$getinverse()
    if (!is.null(x_inverse)) {
        message("Getting cached matrix inverse...")
        return(x_inverse)
    } else {
        x_inverse <- solve(x$get())
        x$setinverse(x_inverse)
        x_inverse
    }
}
