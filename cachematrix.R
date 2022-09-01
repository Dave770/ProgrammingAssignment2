## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(matrix = matrix()) {
  inverse <- NULL
  set <- function(y) {
    matrix <<- y
    inverse <<- NULL
  }
  get <- function() matrix
  set_inverse <- function(inv) inverse <<- inv
  get_inverse <- function() inverse
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$set_inverse(inverse)
  inverse
}
