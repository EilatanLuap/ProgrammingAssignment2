##makes 2 functions (makeCacheMatrix and cacheSolve) that cache the inverse of a matrix. 

## Creates a Matrix object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
inv <-  NULL
set <-  function(y) {
  x <<- y
  inv <<- NULL
}
get <-  function() x
setinverse <-  function(inverse) inv <<- inverse
getinverse <-  function() inv
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Computes the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  return(inv)
}

#tests the functions above
A <- matrix(c(3,4,3,7), nrow = 2,ncol = 2)
A
A1 <- makeCacheMatrix(A)
cacheSolve(A1)
           