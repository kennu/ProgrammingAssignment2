## Functions for computing cached matrix inversions

## makeCacheMatrix creates a cached matrix object for storing cached inversion results
## Parameter matrixValue is the initial matrix stored in the cache
## Use cacheSolve() to compute its inverse

makeCacheMatrix <- function(matrixValue = matrix()) {
  inverseValue <- NULL
  
  # Sets the matrix whose inverse is cached (and resets the cached inverse)
  set <- function(newMatrixValue) {
    matrixValue <<- newMatrixValue
    inverseValue <<- NULL
  }
  
  # Gets the matrix whose inverse is cached
  get <- function() {
    matrixValue
  }
  
  # Sets the computed inverse value of the matrix
  setinverse <- function(newInverseValue) {
    inverseValue <<- newInverseValue
  }
  
  # Gets the computed inverse value of the matrix
  getinverse <- function() {
    inverseValue
  }
  
  # Return the cached matrix object as a vector of functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve computes the inverse of the given matrix, using cached results when available
## Parameter cacheMatrix must be a cached matrix object created using makeCacheMatrix()

cacheSolve <- function(cacheMatrix, ...) {
  # Check if solution is already cached
  inverseValue <- cacheMatrix$getinverse()
  if (!is.null(inverseValue)) {
    # Yes, solution was cached, return it
    return(inverseValue)
  }
  
  # No, solution not yet cached, compute it now and cache it
  matrixValue <- cacheMatrix$get()
  inverseValue <- solve(matrixValue, ...)
  cacheMatrix$setinverse(inverseValue)
  
  # Return the computed inverse matrix
  inverseValue
}
