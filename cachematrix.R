## Kristin Chen Programming Assignment 2 
## JUNE 21, 2015

## Part 1: makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    m << y
    inverse <<- NULL
  }
  get <- function() m
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Part 2: cacheSolve
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
          ## Return a matrix that is the inverse of 'x'
  matrix <- x$get()
  inver <- x$getinverse
  #does matrix == x and... does inverse exist?????
  if (matrix == x && !is.null(inver)) {
    return inver
  }
  
  ## if this doesn't exist, then you can calculate the inverse and set it after
    inver <- solve(matrix)
    x$setinverse(inver)
    return inver
}
