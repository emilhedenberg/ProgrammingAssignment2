## Functions to create a cachable matrix, and to perform a solve inversion on such a matrix

## Create a new matrix that is cachable, holds a field for a solved matrix
## Includes getters and setters for value object and solved object
makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) s <<- solve
	getsolve <- function() s
	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Perform Inversion with solve on a matrix
## If it has been inverted, return that value from cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	s <- x$getsolve()
	if(!is.null(s)) {
		message("getting cached data")
		return(s)
	}
	data <- x$get()
	s <- solve(data)
	x$setsolve(s)
	s
}
