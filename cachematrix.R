## The functions below are used to set, retrieve and cache the calculation of inverting a matrix, to speed up subsequent retrievals.

## create a function which allows us to 
##  1. set/get the value of our original matrix
##  2. set/get the value of the inverse of the matrix (without actually computing it. this is done in the cacheSolve function)
##  3. will cache any values suppied in "set" operation
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Takes the makeCacheMatrix object as a parameter, and returns the matrix inverse.
## Will take the cached version from makeCacheMatrix if available. If not will perform the calculation, return the inverse and cache the result.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
