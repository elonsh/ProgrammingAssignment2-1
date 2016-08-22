## These functions create a cache that take the inverse of a matrix. 

## This first function creates a matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = gerinverse)
  
}


## This function calculates the inverse. It first checks 
## the environment to see if it has already been created.
## If it finds it, it skips the calucation and grabs the 
##previous result.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat, ...)
  x$setInverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}