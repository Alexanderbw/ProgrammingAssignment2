## These two functions enable a user to compute and cache the inverse of a 
## matrix

## Function makeCacheMatrix takes as input a matrix and outputs a list, L,  of 
## four functions: set, get, setSolve and getSolve
## These functions will be used to store and return a matrix, m, and its 
## inverse, inv.

makeCacheMatrix <- function(m = matrix()) {
      inv <- NULL
      set <- function(y) {
            m <<- y
            inv <<- NULL
      }
      get <- function() m
      setSolve <- function(solve) inv <<- solve
      getSolve <- function() inv
      list( set = set, get = get,
            setSolve = setSolve,
            getSolve = getSolve )
}


## Function cacheSolve takes as input the list, L, that is output by 
## makeCacheMatrix.
## If the L contains the cached inverse of m, this is returned.
## Otherwise, the inverse of m, inv, is calculated, stored L, and returned.  

cacheSolve <- function(x, ...) {
      m <- x$getSolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setSolve(m)
      m
}