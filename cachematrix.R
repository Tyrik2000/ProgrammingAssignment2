## Reading the matlib package so I can use the inverse function
library(matlib)

## MakeCacheMatrix function sets and gets the value of the square matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) { 
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inv(x)  
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve function calculates the inverse of the matrix and returns the cache from makeCacheMatrix

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- Solve(data, ...)
    x$setinverse(m)
    m
}

