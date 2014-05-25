## creates a special "matrix", which contains matrix and it's inverse with following methods
## set: set the value of the matrix
## get: get the value of the matrix
## set_inverse: set the value of the matrix
## get_inverse: get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get <- function() x
    set_inverse <- function(val) inverse <<- val
    get_inverse <- function() inverse
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
  }



## takes special "matrix" object created using makeCacheMatrix as input
## checks if inserse is already there, if not then calculate inverse and caches it
## assuming matrix supplied will always be invertible, there's no check for that

cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'}
    m <- x$get_inverse()
    if(!is.null(m)) {
          ## message to depict inverse is fetched from cache rather than recalculating it
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$set_inverse(m)
    m
  }