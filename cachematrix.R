## The following two functions have been written for the second programming
## assignment in the R Programming course.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list (set=set, get=get,
        setmatrix = setmatrix,
        getmatrix = getmatrix)
}


## This function computes the inverse of the special "matrix" returned by the
## makeCacheMatrix function.  If the inverse has already been calculated (and the
## matrix has not changed), then the cacheSolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x = matrix(), ...) {
  m <- x$getmatrix()
  if (!is.null(m)) {
    message("getting cached data")
    return (m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}
