## These functions create a special matrix object that can cache its inverse.
## makeCacheMatrix stores a matrix and its cached inverse.
## cacheSolve computes the inverse of the matrix, retrieving the cached value
## if it has already been calculated and the matrix has not changed.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" created by
## makeCacheMatrix. If the inverse has already been calculated, it retrieves
## the cached value instead of recomputing it.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}

