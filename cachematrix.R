## Author: Brian McNally
## Version: 1.0
## Date: 2014.12.13
## This R Module is for creating a matrix "super object" which contains both the matrix and 
## its inverse.
## Function makeCacheMatrix() is the constructor and contains access API
## Function cacheSolve() MUST be called to populate inverse matrix on makeCacheMatrix object

## Creates a matrix "super object" which contains both the matrix and its inverse
## Input: Matrix()
## Access functions:
##  get(): Return the input matrix
##  setinverse(): <PRIVATE> Set the inverse of the matrix
##  getinverse(): Return the inverse of the matrix or NULL if not set
##  set(): Sets a new matrix to the object and sets inverse to NULL

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  
  get <- function()
  {
    x
  }
  
  setinverse <- function(invertedMatrix)
  {
    cachedInverse <<- invertedMatrix
  }
  
  getinverse <- function()
  {
    cachedInverse
  }
  
  list(get = get, 
       set = set, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## CacheSolve is called to set the inverse of the makeCacheMatrix object
## Input: Object result of function makeCacheMatrix
## If the inverse of the matrix has already been stored, cachesolve will 
## print a message and return the stored result
## Otherwise, it will calculate, store, and return the inverse

cacheSolve <- function(x, ...) {
  
  
  storedInverse <- x$getinverse()
  
  if(!is.null(storedInverse))
  {
    message("getting cached data")
    return(storedInverse)
  }
  
  # The next three lines: Get the matrix data, Set the cacheInverse to the Inverse, Return
  matrixData <- x$get()
  x$setinverse(solve(matrixData))
  return(x$getinverse())
}
