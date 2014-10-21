## The pair of functions set and solve the inverse of a Matrix (assuming is invertible) and stores in cache the result.  
## If we ask to solve the same matrix, the result will come from the cache.

## makeCacheMatrix is a function that set a matrix,  and solve the inverse for it. Additionally, stores the result in cache, 
##where the final result is a list. 
##

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 

  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {x}
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  li <- list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## CacheSolve is a function that either recover from cache the computed inverse matrix in the last attempt or calculate the inverse
## if a new matrix is aksed to solve by CacheSolve.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
