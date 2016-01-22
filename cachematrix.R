## Put comments here that give an overall description of what your
## functions do
##
## Write a short comment describing this function
## makeCacheMatrix takes a matrix as input and returns a list of 
## functions in elements named with the function name that can be 
## used to set or retrieve the matrix (set and get resp.) or its 
## inverse (setInv and getInv resp.)
## Setting the matrix resets the cache.
##
makeCacheMatrix <- function(x = matrix()) {
      minv <- NULL
      set <- function(ms) {
            x <<- ms
            minv <<- NULL
      }
      get <- function() x
      setInv <- function(inv) minv <<- inv
      getInv <- function() minv
      list(set = set, get = get,
           setInv = setInv,
           getInv = getInv)
}

## Write a short comment describing this function
## cacheSolve takes the functions list returned by makeCacheMatrix
## and returns the inverse of the matrix - checking and returning 
## the inverse if it has been cached, or solving for the inverse 
## and caching the inverse. The inverse is returned in either case.
##

cacheSolve <- function(x, ...) {
      mInverse <- x$getInv()
      if(!is.null(mInverse)) {
            message("getting cached inverse")
            return(mInverse)
      }
      mat <- x$get()
      mInverse <- solve(mat, ...)
      x$setInv(mInverse)      
      ## Return a matrix that is the inverse of 'x'
      mInverse
}