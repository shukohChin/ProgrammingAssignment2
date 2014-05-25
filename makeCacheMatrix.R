makeCacheMatrix <- function(x = matrix()) {
  # makeCacheMatrix creates a special "matrix"
  # which is really a list containing a function to
  # 1. set the value of the matrix
  # 2. get the value of the matrix
  # 3. set the inverse of the matrix
  # 4. get the inverse of the matrix
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

cacheSolve <- function(x, ...) {
  # create inverse of the matrix created with "makeCacheMatrix"
  # It first check to see if the inverse of the matrix has already been calculated.
  # If so, it gets the result from the cache and skips the computation.
  # Otherwise, it computes the inverse of the matrix and set the value in the cache via the setMatrix function.
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