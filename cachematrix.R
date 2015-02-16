## This program calculates the inverse of a square matrix. Since the calculation
## can be intensive, the result is cached.
## The function makeCacheMatrix creates a list of functions to get/set a matrix
## and get/set an inverse matrix.
## The function cacheSolve gets the inverse matrix from the cache. If the result
## is null (hence not stored in the cache) the inverse is calculated and set.
##
## Author: Daniel S. Plant
## Version: 1.0
## Date: 16-FEB-2015

## This function produces a list of functions to set a matrix ("set"), retrieve a 
## matrix ("get"), set an inverse matrix ("setInverse"), and retrieve an 
## inverse matrix ("getInverse"). The matricies are stored/retrieved in the cache.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <- y
            m <- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## This function takes the output of the function makeCacheMatrix. It retrieves
## the inverse matrix from the cache. If this is null it caculates the inverse
## matrix using the function "solve".

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()
      if(!is.null(inv)) {
            message("Getting cached data...")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setInverse(inv)
      inv
}
