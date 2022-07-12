## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Create a object like function, that contains the matrix value
# and inverse value, the latter will remain the same (if value set) until 
# matrix is changed (set is called)
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  get <- function() x
  set <- function(new) {
    inverse <<- NULL
    x <- new
  }
  
  getInverse <- function() inverse
  setInverse <- function(inv) inverse <<- inv
  
  list(get=get, set=set, getInverse=getInverse, setInverse=setInverse)
}


## Write a short comment describing this function
# A function that checks if a (environment) value is set for matrix inverse
# if it has, then already set value is returned, else the inverse matrix is
# calculated, set to environment and returned to the user
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  i <- solve(x$get(), ...)
  x$setInverse(i)
  i
}
