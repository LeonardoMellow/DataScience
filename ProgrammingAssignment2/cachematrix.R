## This function creates a special "matrix", which is really a list containing a
## function to set the value of the matrix, get the value of the matrix, set the
## inverse of the matrix and get the inverse of the matrix

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

## The next function calculates the inverse of the special "matrix" created with 
## the previous function. It first checks to see if the inverse has already been
## calculated. If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it calculates the inverse and sets the value of the
## inverse in the cache via the setSolve function. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
