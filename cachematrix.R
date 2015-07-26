## cachematrix.R
## John DeCuir
##
## This file contains two functions, makeCacheMatrix and cacheSolve,
## in accordance with programming assignment 2 of R Programming
## on Coursera.

## makeCacheMatrix creates a special "matrix" which is a list
## that contains functions to set the value of the matrix,
## get the value of the matrix, set the inverse of the matrix,
## and get the inverse of the matrix.
##
## Arguments: The matrix to store in this list (this can be
##            set later via the 'set' function returned by
##            this list)
## Return value:  the special "matrix"
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve calculates the inverse of the special "matrix"
## created by makeCacheMatrix.  It first checks to see if
## the inverse has already been calculated.  If so, it
## simply returns that inverse matrix.  Otherwise, it calculates
## the inverse and stores it in the special object, and then
## returns the inverse.
##
## Arguments: The special "matrix" created by makeCacheMatrix.
## Return value: The inverse of the matrix.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i))
  {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
