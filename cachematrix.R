## This R script containts two functions that are used to create a special
## object that stores a matrix, M, and caches its inverse, M'. 


## makeCacheMatrix is a function that creates a special "matrix" object,  
## which can be used to cache the inverse, M', of the original matrix, M, 
## in combination with the function below (cacheSolve). When the function
## is used to cache the inverse of a new matrix, the special "matrix" 
## object is first reset to "NULL" before being recomputed.

makeCacheMatrix <- function(x = matrix()) { 
        m <- NULL                                 
        set <- function(y) {                      
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve is a function that computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed since that calculation), then 
## the cachesolve should retrieve the inverse from the cache. If the matrix
## has changed, then the inverse of the new matrix ix calculated and cached.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
