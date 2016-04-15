## The following function calculates the Inverse of the special 
## "matrix". It first checks to see if the Inverse has already been 
## calculated. If so, it gets the Inverse from the cache and skips 
## the computation


## This function creates a special "matrix" object that can 
## cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize
  m <- NULL
  
  # Set matrix object
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Get matrix object
  get <- function() x
  
  # Set inverse values
  setSolve <- function(solve) {
    m <<- solve
  }
  
  # Get inverse values
  getSolve <- function() {
    m
  }
  
  # Create list as a return object
  list(set = set, 
       get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  # Retrieve inverse values
  m <- x$getSolve()
  
  # Check if inverse values exist then return
  if(!is.null(m)) {
    message("Getting cached data..")
    return(m)
  }
  
  # Get the matrix and compute inverse
  data <- x$get()
  m <- solve(data, ...)
  
  # Set the inverse values in the cache
  x$setSolve(m)
  
  # Return a matrix that is the inverse of 'x'
  m  
}
