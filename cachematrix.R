## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# below method contains matrix and it's inverse value 
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



## Write a short comment describing this function
# checks if inserse is already there, if not then calculate inverse and save/cache it
cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'}
    m <- x$get_inverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$set_inverse(m)
    m
  }