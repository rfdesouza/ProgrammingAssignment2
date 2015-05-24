## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix takes a matrix as its argument.
## It consists of four functions: set, get, setInverse and getInverse
## set: sets the value of the matrix
## get: gets/returns the value of the matrix
## setInverse: sets the value of the inverse matrix
## getInverse: gets/returns the value of the inverse matrix
## Calling makeCacheMatrix() returns a list of each of the above function's
## source code.

makeCacheMatrix <- function(aMatrix = matrix()) {
  inverseMatrix <- NULL
  set <- function(newMatrix) {
    aMatrix <<- newMatrix
    inverseMatrix <<- NULL
  }
  get <- function() aMatrix
  setInverse <- function(solveResult) inverseMatrix <<- solveResult
  getInverse <- function() inverseMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

## cacheSolve takes the function makeCacheMatrix as its argument.
## It first checks to see if the getInverse function in makeCacheMatrix returns
## a value (i.e. the value of the inverse matrix). If a value is returned, it
## returns the inverse matrix from the cache and skips the solve operation.
## Otherwise, it gets the matrix from makeCacheMatrix, solves it, passes the
## solution (i.e. the inverse matrix) to the setInverse function in
## makeCacheMatrix, and returns the value of the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setInverse(inverseMatrix)
  inverseMatrix
}
