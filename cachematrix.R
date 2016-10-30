## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
##      of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion 
##      that we will not discuss here). Your assignment is to write a pair of functions that cache the 
##      inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invMat <- NULL
    setMat <- function(y) {
        x <<- y
        invMat <<- NULL
    }
    getMat <- function() x
    setInvMat <- function(inverse) invMat <<- inverse
    getInvMat <- function() invMat
    list(setMat=setMat, getMat=getMat, setInvMat=setInvMat, getInvMat=getInvMat)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##      If the inverse has already been calculated (and the matrix has not changed), 
##      then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInvMat()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$getMat()
    inv <- solve(data)
    x$setInvMat(inv)
    inv
}

## Define a square inversible matrix
x = matrix(c(1, 5, 6, 7, 8, 3, 2, 5, 7), nrow = 3, ncol = 3)

## Set Cache Matrix to m
m <- makeCacheMatrix(x)

## Set Matrix to upper environment
m$setMat(x)

## Get Matrix from upper environment
m$getMat()

## inverse Matrix from upper envionement
cacheSolve(m)


## Validation that cacheSolve shows the same data as m$getInvMat()
m$getInvMat()

