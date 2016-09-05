## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix creates a list containing a function to
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of inverse of the matrix
## 4. gets the value of inverse of the matrix

## Created - 9/4/2016

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ## Sets the value of matrix
    set <- function(y) {                
        x <<- y
        inv <<- NULL
    }
    ## gets the value of the matrix
    get <- function() x
    ## Sets the value of the inverse of the matrix
    sinv <- function(inverse) inv <<- inverse
    ## gets the value of the inverse of the matrix
    ginv <- function() inv
    list(set=set, get=get, sinv=sinv, ginv=ginv)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$ginv()
    ## if already in cache - returns cached information
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    ## if not in memory computes the inverse in memory
    inv <- solve(data)
    x$sinv(inv)
    inv
}
