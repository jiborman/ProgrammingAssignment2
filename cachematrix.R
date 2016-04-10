## R programming assignment 2
# First function, makeCacheMatrix defines functions 
# to hold matrix and inverse in cache. Second function, cacheSolve does
# the inversion of a matrix (square only)

## Generates list of four functions
## * set: set the value of a matix
## * get: get the value of the matrix
## * setInverse: set the inverse of the matrix
## * getInverse: get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  # nothng in the cache to start
  i <- NULL
  
  # if new value, reset the cache
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # set the inverse of the matrix into the cache
  setInverse <- function(mean) i <<- mean
  
  # get the inverse of the matrix from the cache
  getInverse <- function() i
  
  # return list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Either pulls a cached inverse of a matrix x, or computes
## the inverse, stores it in the cache, and returns that

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # grab the inverse from the cache
  m <- x$getInverse()
  
  # does cache have an inverse in it?
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # otherwise, do the inversion of the the matrix
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
