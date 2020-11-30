## Put comments here that give an overall description of what your
## functions do

## Modified makeVector function for Coursera assignment
## Allows Creation of makeCacheMatrix object with various 'methods'

makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(y) {
    mat <<- y
    inv <<- NULL
  }
  get <- function() mat
  setInverse <- function(solved) inv <<- solved
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Checks if inverse exists, if it does, it returns the inverse.
## If inverse doesn't exist, it uses the object's methods to set one

cacheSolve <- function(mat, ...) {
  inv <- mat$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- mat$get()
  inv <- solve(data, ...)
  mat$setInverse(inv)
  inv
}
