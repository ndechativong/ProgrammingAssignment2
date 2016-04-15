## The following function calculates the Inverse of the special 
## "matrix". It first checks to see if the Inverse has already been 
## calculated. If so, it gets the Inverse from the cache and skips 
## the computation


## This function creates a special "matrix" object that can 
## cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("Getting cached data..")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  
  m  ## Return a matrix that is the inverse of 'x'
}
