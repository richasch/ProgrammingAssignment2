## Caching the Inverse of a Matrix

## makeCacheMatrix
## Input:  param_matrix   An invertable matrix
## This function creates an object that wraps a matrix with get and 
## set functions to store the inverse of a matrix using the solve function 

makeCacheMatrix <- function(param_matrix = matrix()) {

        m <- NULL
        set <- function(y) 
		{
                param_matrix <<- y
                m <<- NULL
        }
        get <- function() param_matrix
        setinvert <- function(invert) m <<- invert
        getinvert <- function() m
        list(set = set, get = get,
             setinvert = setinvert,
             getinvert = getinvert)
}


## cacheSolve
## Input:  param_cacheMatrix   A matrix object created by the makeCacheMatrix function
## This function calculates and saves the inverse of a matrix and if called with the 
## same cache matrix object, returns the inverse from cache rather than recalculating

cacheSolve <- function(param_cacheMatrix, ...) {
        ## Return a matrix that is the inverse of 'param_cacheMatrix'
        m <- param_cacheMatrix$getinvert()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- param_cacheMatrix$get()
        m <- solve(data, ...)
        param_cacheMatrix$setinvert(m)
        m
}
