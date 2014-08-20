## Functions to wrap matrixes and provide caches matrix inversion.
## This can be used to avoid repeated calls to solve(matrix)
## which may improve performance. The matrix to be wrapped must be invertible.
## Usage:
## wrapped_x <- makeCacheMatrix(x)
## inverse <- cacheSolve(wrapped_x)


## Create a wrapper around a matrix as a list of four functions:
## set: store a matrix
## get: retrieve the stored matrix
## setinverse: store the inverse of the matrix previously stored
## getinverse: retrieve the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inverseX <- NULL
  set <- function(y) {
    x <<- y
    inverseX <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverseX <<- inverse
  getinverse <- function() inverseX
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)    
}


## Return the inverse of the matrix wrapped by x.
## If a cached result is present it will be returned.
## Otherwise the matrix will be inverted using solve()
## and the result will be cached.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse  
}
