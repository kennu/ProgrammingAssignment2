## Functions for computing cached matrix inversions

## makeCacheMatrix creates a cached matrix object for storing cached inversion results
## Parameter x is the initial matrix stored in the cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Sets the matrix whose inverse is cached
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Gets the matrix whose inverse is cached
  get <- function() {
    x
  }
  
  # Sets the inverse value of the matrix
  setinverse <- function(i) {
    inv <<- i
  }
  
  # Gets the inverse value of the matrix
  getinverse <- function() {
    inv
  }
  
  # Return the object as a vector of functions
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve computes the inverse of the given matrix x, using cached results when available
## Parameter x must be a cached matrix object created using makeCacheMatrix()

cacheSolve <- function(x, ...) {
  # Check if solution is cached
  inv <- x$getinverse()
  if (!is.null(inv)) {
    # Yes, it was cached, return it
    return(inv)
  }
  
  # No, solution not yet cached, compute it now and cache it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  # Return the inverse matrix
  inv
}
