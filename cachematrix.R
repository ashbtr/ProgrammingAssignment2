## My first function is a nested function that uses lexical scoping
## to contain 'get' and 'set' functions that allow caching of 
## matrix inversions. The second function inverts the matrix if a 
## cached value is not present

## The makeCacheMatrix function contains two objects and four
## nested functions. These nested functions allow the 
## input matrix and its inversion to be cached in the 
## parent environment.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function()m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function checks for a cached inversion of the input
## and if there is no cached value, it will compute one

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    m
}
