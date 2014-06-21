## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that has the ability
## to cache its inverse so that it avoids recomputation
makeCacheMatrix <- function(x = matrix()) {
	
	#this object stores two matrices and has 4 methods
	
	#this is the value of the inverse of the matrix
	inv <- NULL
	
	# this sets the matrix value
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	# this gets the current matrix value
	get <- function() x
	
	# this sets the inverse of the matrx and caches it in external environment
	setInverse <- function(inverse) inv <<- inverse
	
	# this gets the inverse of the matrix, from cache if it exists
	getInverse <- function() inv
	
	#return the object definition
	list(set = set, get = get,
		  setInverse = setInverse,
		  getInverse = getInverse)
}


## This function inverts a matrix object that has caching ability

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	# see if there is already an inverse calculated
	inv <- x$getInverse()
	if(!is.null(inv)) {
		#for debug purposes, put out a msg that shows we are using the cache
		message("getting cached data")
	} else {
		#compute the inverse and cache it
		inv <- x$setInverse(solve(x$get(), ...))
	}
	inv
}
