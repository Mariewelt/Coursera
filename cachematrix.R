## The first function, makeCacheMatrix creates a special "matrix", which is really 
## a list containing a function to
## -set the value of the matrix
## -get the value of the matrix
## -set the value of the inverse matrix
## -get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL  
}
get <- function() x
setinv <- function(solve) x_inv <<- solve
getinv <- function() x_inv
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
}


## The following function calculates the inverse matrix of the special "matrix" created with 
## the above function. However, it first checks to see if the inverse matrix has already 
## been calculated. If so, it gets the inverse matrix from the cache and skips 
## the computation. Otherwise, it calculates the inverse matrix of the data matrix 
## and sets the value of the inverse matrix in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  x_inv <- x$getinv()
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data, ...)
  x$setinv(x_inv)
  x_inv
}