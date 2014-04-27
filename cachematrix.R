## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## Initialize the cached inverse
    i <- NULL 
    
    set <- function(y) {
        ## A new matrix is set, no inverse calculated yet, initialize to NULL
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(inv) {i = inv}
    
    getinv <- function() i
    
    list(set = set, get = get, 
         setinv = setinv, 
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if (!is.null(i)) {
        ## inverse has been calculated and been cached.
        message("getting cached data")
        return (i)
    }
    
    ## Not calculated before, use solve function to calculate it.
    data <- x$get()
    i <- solve(x, ...)
    ## cache it.
    x$setinv(i)
    i
}
