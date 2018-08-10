# This function create special structure (list) with following arguments:
# s: cached matrix inversion (result of solve function)
# set: function to set matrix
# get: function to get matrix
# setsolve: function to set matrix inversion
# getsolve: function to get matrix inversion

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

# This function calculates the inversion of matrix.
# Firstly, it checks if inversion already was calculated.
# If yes: return data from cache
# else: calculate inversion, write it to special structure, created with the above function
# and return it.
cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if (!is.null(s)) {
        message("Getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}