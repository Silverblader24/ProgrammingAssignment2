## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## A pair of functions that cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  i <- NULL
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }

  get <- function() {
    m
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  getInverse <- function() {
    i
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  
  
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  m <- solve(data) %*% data
  x$setInverse(m)
  m
}
