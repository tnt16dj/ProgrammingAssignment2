## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly. This 
## program creates a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(calculatedInverse) inverse <<- calculatedInverse
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        passedMatrixInverse <- x$getInverse()
        if(!is.null(passedMatrixInverse)) {
                message("getting cached data")
                return(passedMatrixInverse)
        }
        data <- x$get()
        passedMatrixInverse <- solve(data, ...)
        x$setInverse(passedMatrixInverse)
        passedMatrixInverse
}

## example runs of the above functions...
## > source('~/.active-rstudio-document')
## > uMatrix <- makeCacheMatrix(matrix(c(1,2,2,1),nrow=2,ncol=2))

## this is the initial cacheSolve call, no inverse has been calculated thus far

## > cacheSolve(uMatrix)
## [,1]             [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333

## this is the second cacheSolve call, an inverse has been stored and the Matrix 
## being passed in has not changed... notice "getting cached data" indicating the
## cache stored value is being used.

## > cacheSolve(uMatrix)
## getting cached data
## [,1]             [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333

## now we'll change the uMatrix

## > uMatrix <- makeCacheMatrix(matrix(c(1,3,3,1),nrow=2,ncol=2))

## this is the third cacheSolve call, notice "getting cached data" is not printed.
## the matrix has changed and reset the inverse to NULL

## > cacheSolve(uMatrix)
## [,1]         [,2]
## [1,] -0.125  0.375
## [2,]  0.375 -0.125

## this is the fourth cacheSovle call, "getting cached data" is printed again
## as the function is retrieving the previously stored inverse from call three...

## > cacheSolve(uMatrix)
## getting cached data
## [,1]         [,2]
## [1,] -0.125  0.375
## [2,]  0.375 -0.125
