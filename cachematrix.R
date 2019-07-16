## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  get <- function() x
  setinvMat <- function(inverse) invMat <<- inverse
  getinvMat <- function() invMat
  list(set = set, get = get, setinvMat = setinvMat, getinvMat = getinvMat)
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If
#the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve
#the inverse from the cache.

cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
  invMat <- x$getinvMat()
  if(!is.null(invMat)) {
    message("getting cached result")
    return(invMat)
  }
  data <- x$get()
  invMat <- solve(data, ...)
  x$setinvMat(invMat)
  invMat
}







