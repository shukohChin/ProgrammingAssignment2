## These functions cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrix <- function(solve) m <<- solve
  getMatrix <- function() m
  list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)
}


## create inverse of the matrix created with "makeCacheMatrix"
## It first check to see if the inverse of the matrix has already been calculated.
## If so, it gets the result from the cache and skips the computation.
## Otherwise, it computes the inverse of the matrix and set the value in the cache via the setMatrix function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setMatrix(m)
  m  
}
