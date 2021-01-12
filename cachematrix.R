#This function takes a matrix, returns a list of functions

makeCacheMatrix <- function(x = matrix()) {
    rev <- NULL
    set <- function(y) {
        x <<- y
        rev <<- NULL #New matrix, so clearing the previously saved cache
    }
    get <- function() {
        x
    }
    setReverse <- function(reverse) {
        rev <<- reverse
    }
    getReverse <- function() {
        rev
    }
    list(set = set,
         get = get,
         setReverse = setReverse,
         getReverse = getReverse
         )
    #Returning a list of functions that will be useful to cacheSolve
}


## This function will look for the cached value of reversed matrix
## If not found, then it will calculate the reverse and save in cache
## Finally it will return the reverse of matrix x

cacheSolve <- function(x, ...) {
    rev <- x$getReverse()
    if(!is.null(rev)) {
        message("Getting cached data")
        return(rev)
        #Already data is cached,
        #so need to recalculate, just returning the cached value
    }
    rawMat <- x$get() #getting the matrix
    rev <- solve(rawMat) #inverting the matrix
    x$setReverse(rev) #Saving the value in the cache
    return(rev)
}
