## cachematrix contains functions to compute, store and retrieve the inverse of a square matrix from mem cache. 

## makeCacheMatrix takes as input a square matrix and returns a list of function objects that can be passed to cacheSolve to perform caching or retrieval of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setmatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getmatrix <- function() x
        setmatrixinverse <- function(mean) m <<- mean
        getmatrixinverse <- function() m
        list(setmatrix = setmatrix, 
			 getmatrix = getmatrix,
             setmatrixinverse = setmatrixinverse,
             getmatrixinverse = getmatrixinverse)
}

## cacheSolve returns the inverse of a cached square matrix defined in an input function list defined in makeCacheMatrix.
## If the matrix inverse has previously been cached cacheSolve will return the cached version, otherwise it computes the inverse of the matrix from the original cached matrix  
## Optional arguments are currently ignored.

cacheSolve <- function(x,...) {
        m <- x$getmatrixinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getmatrix()
        m <- solve(data)
        x$setmatrixinverse(m)
        m
}
