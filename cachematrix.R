## Put comments here that give an overall description of what your
## functions do

## this function will create the matrix and cache it

makeCacheMatrix <- function(x = matrix()) {
if (ncol(x) == nrow(x) && det(x) != 0) {
    m <- NULL
    
    # Setter function to set the matrix
    set <- function(y) {
      x <<- y
      m <<- NULL
}
# Getter function to retrieve the matrix
    get <- function() x
    
    # Function to compute and cache the inverse
    setinverse <- function() m <<- solve(x)
    
    # Function to retrieve the cached inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  } else {
    stop("The matrix is not invertible.")
  }



## this function will solve the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
  if (!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
