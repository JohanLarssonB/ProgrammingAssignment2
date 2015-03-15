## Two functions that demonstrate caching of data.  
## The inverse of a matrix is calculated and stored together
## with the matrix in a list. If there is subsequent calls 
## to the function that calculates the inverse, and the matrix
## has not changed, the cached inverse is used instead of
## re-caluclating the inverse again.

## makeCacheMatrix is the constructor of the list with four
## functions to set and get the matrix and the inverse.

makeCacheMatrix <- function(x = matrix()) {
  
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheInverse calculates the inverse and sets the inverse
## in the list unless the inverse is already calculated, in
## which case the cached inverse is used.

cacheInverse <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if (!is.null(i)) {
        message("Getting cahed data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
