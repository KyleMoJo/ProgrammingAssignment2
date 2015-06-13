## These two functions have been adapted from the code for functions "makeVector" 
## and "cacheMean" provided in the assignment description. Their purpose is to 
## provide a "vector"-type object that stores a matrix and can cache its inverse,
## so that the inverse is only calculated once and then retrieved from the cached
## value, provided that the matrix has not been changed.

## makeCacheMatrix stores a matrix and caches its inverse (when this has been
## calculated and cached by the cacheSolve function, below).

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve checks if an inverse matrix has been cached in makeCacheMatrix, and
## returns this inverse if it has been cached. If there is no cached inverse, 
## then it retrieves the matrix, calculates the inverse, and caches the inverse.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv    ## Returns a matrix that is the inverse of 'x'
}
