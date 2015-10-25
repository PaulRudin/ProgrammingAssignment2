## Put comments here that give an overall description of what your functions do

## This pair of functions provide a matrix wrapper, called a CacheMatrix,
## allowing for caching of the inverse of the matrix; thereby avoiding the need
## to recalculate the matrix inverse.

## A new wrapper should be created using makeCacheMatrix. The inverse can be
## calculated using cacheSolve; which will retrieve a cached value rather then
## recalculating when appropriate. See the descriptions of the individual
## functions for more detail.

## Example use:
## > x <- matrix(rexp(9), 3) # create a 3x3 matrix of random numbers
## > x
##           [,1]     [,2]      [,3]
## [1,] 1.9246665 1.063439 1.6219225
## [2,] 0.4753947 1.781132 1.0126000
## [3,] 1.1319229 2.976365 0.1504967
## > cx <- makeCacheMatrix(x) # create a new CacheMatrix initialised with x
## > cx$get()     # check that the underlying matrix is x
##           [,1]     [,2]      [,3]
## [1,] 1.9246665 1.063439 1.6219225
## [2,] 0.4753947 1.781132 1.0126000
## [3,] 1.1319229 2.976365 0.1504967
## > cacheSolve(cx) # get the inverse - note: no hit report
##            [,1]       [,2]       [,3]
## [1,]  0.5366072 -0.9121360  0.3541184
## [2,] -0.2100141  0.3021768  0.2301869
## [3,]  0.1174824  0.8842661 -0.5711429
## > cacheSolve(cx) #get the inverse again
## getting cached data  # note the hit report
##            [,1]       [,2]       [,3]
## [1,]  0.5366072 -0.9121360  0.3541184
## [2,] -0.2100141  0.3021768  0.2301869
## [3,]  0.1174824  0.8842661 -0.5711429
## > x %*% cacheSolve(cx)    # check that the inverse is correct.
## getting cached data
##              [,1]          [,2] [,3]
## [1,] 1.000000e+00  0.000000e+00    0
## [2,] 0.000000e+00  1.000000e+00    0
## [3,] 6.938894e-18 -8.326673e-17    1
## > 


## Write a short comment describing this function

## Create a new CacheMatrix, which provides the following functions:

## get() - the current matrix. The matrix can be provided when creating the
## CacheMatrix or set using set()

## set() - sets the current matrix. This is bound to the name 'x' in the
## execution environment of the creating call to makeCacheMatrix. This also
## invalidates the current cached value.

## setmatrix() - sets the inverse of the matrix; this is bound to the name 'm'
## in the execution environment of the creating call to makeCacheMatrix.

## getmatrix() - retrieves the inverse of the matrix 

## Note that setmatrix() and getmatrix() are primarily intended for use by
## cacheSolve; it is not recommended that these are used directly.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL # new CacheMatrix, inverse not calculated yet
    set <- function (y) {
        x <<- y
        m <<- NULL # new matrix - invalidate cached value
    }
    get <-  function () x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function () m
    list (set = set,
          get = get,
          setmatrix = setmatrix,
          getmatrix = getmatrix)
}

## Write a short comment describing this function

## This function take a CacheMatrix created by makeCacheMatrix and returns the
## inverse of the matrix. If a cached value already exists in the CacheMatrix
## this is returned; otherwise the inverse is computed and stored in the
## CacheMatrix before being returned.

## the parameters after x are passed through to solve when calculating the
## matrix inverse.

## For diagnostic purposes, cacheSolve reports cache hits.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if (!is.null(m)) {
        message("getting cached data")
        return (m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}
