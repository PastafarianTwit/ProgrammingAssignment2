## This file stores a cache of a matrix inversion and reuses the cached
## version if it's already been calculated.  This reduces the overhead
## required for large matrices.

## a list of helper functions for caching the matrix inversion

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setSolve <- function(solve) s <<- solve
	getSolve <- function() s
	list(set = set, get = get, setmean = setmean, getmean = getmean)
}


## This will perform the matrix inversion, checking the cache first. 
## If no cache is found, it will run the calculation and store to cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		s <- x$getSolve()
		if(!is.null(s)) {
			message("getting cached data")
			return(s)
		}
		data <- x$get()
		s <- solve(data, ...)
		x$setSolve(s)
		s
}
