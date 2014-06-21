## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that has the ability
## to cache its inverse so that it avoids recomputation
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set, get = get,
		  setInverse = setInverse,
		  getInverse = getInverse)
}


## This function inverts a matrix object that has caching ability

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	if(!is.null(inv)) {
		message("getting cached data")

	} else {
		data <- x$get()
		inv <- solve(data, ...)
		x$setInverse(inv)
	}
	inv
}
