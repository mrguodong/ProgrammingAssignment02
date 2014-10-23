## **********************************************************************
## *   R Programming Assignment2 
## *   version 1.0
## *   Last Update 10/24/2014
## *   This function is able to cache potentially time-consuming computations. 
## *   Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 
## *   (there are also alternatives to matrix inversion that we will not discuss here). 
## *    This assignment is to write a pair of functions that cache the inverse of a matrix.

## **************************************************************************
## * Function Name: makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.
## **************************************************************************

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setMatrix <- function(solve) m <<- solve
        getMatrix <- function() m
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)	
}

## **************************************************************************
## * Function Name: cacheSolve
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## **************************************************************************

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get
        m <- solve(data, ...)
        x$setMatrix(m)
        m        
}
