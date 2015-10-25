## Cache the Inverse of a Given Matrix.
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invObject <- NULL
  set <- function(y) {
    x <<- y
    invObject <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invObject <<- inverse
  getInverse <- function() invObject
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the matrix object created by makeCacheMatrix
## The inverse is retrieved from cache if it has already been calculated and the 
## matrix has not changed else it is computed.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invObject <- x$getInverse()
  if (!is.null(invObject)) {
    message("getting cached data")
    return(invObject)
  }
  mat <- x$get()
  invObject <- solve(mat, ...)
  x$setInverse(invObject)
  invObject
}