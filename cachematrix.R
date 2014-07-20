##
## R Programming Assignment 2: Caching the Inverse of a Matrix
##
## Matrix inversion is usually a costly computation and their may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## funcctions, makeCacheMatrix and cacheSolve, cache the inverse of a matrix.
##
## Assumption: the matrix supplied is always invertible
##
## Test Case:
##
## > o <- rbind(c(1,-0.5), c(-0.5,1))
## > o
## [,1] [,2]
## [1,]  1.0 -0.5
## [2,] -0.5  1.0
## > a <- makeCacheMatrix()
## > a$set(o)
## > a$get()
## [,1] [,2]
## [1,]  1.0 -0.5
## [2,] -0.5  1.0
## > cacheSolve(a)
## [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333
## > cacheSolve(a)
## getting cached data
## [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333
## > 


##
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
##
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## setter function to set the value of the vector
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  ## getter function to get the value of the matrix
  get <- function() x
  
  ## setter function to set the value of the inverse matrix
  setInverse <- function(mtrx) m <<- mtrx

  ## getter function to get the value of the inverse matrix
  getInverse <- function() m

  ## name the functions
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


##
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve
## should retrieve the inverse from the cache
##
cacheSolve <- function(x, ...) {
  ## Return a matrix, im, that is the inverse of 'x'

  ## first check if the inverse of the matrix is in cache
  ## assign the returned value to im
  im <- x$getInverse()
  
  ## the inverse is found when im is not NULL
  ## return the cached inverse and skip computation
  if (!is.null(im)) {
    message('getting cached data')
    return (im)
  }
  
  ## get the special "matrix" object in cache
  mtrx <- x$get()
  
  ## compute the inverse and assign it to im
  im <- solve(mtrx)
  
  ## store the inverse to cache
  x$setInverse(im)
  
  ## show the inverse
  im
}