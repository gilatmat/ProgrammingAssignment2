makeCacheMatrix <- function(x = matrix()) {
  value <- NULL
  set <- function(y) {
    x <<- y
    value <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) value <<- solve
  getInverse <- function() value
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Inverse matrix. If inversed, returns cached version.

cacheSolve <- function(x, ...) {
  ## Return inverse of x
  value <- x$getInverse()
  if(!is.null(value)) {
    message("getting cached data")
    return(value)
  }
  data <- x$get()
  value <- solve(data, ...)
  x$setInverse(value)
  value
}