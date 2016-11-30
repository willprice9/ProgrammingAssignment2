## Caching the Inverse of a Matrix:
## This pair of functions will cache the inverse of a matrix rather than having to compute the inverse repeatedly.

## This first function will create a matrix object that will cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## This function computes the inverse of the matrix created above. If the inverse has already been calculated and the matrix remains unchanged, then what will be returned is the inverse from the above cache.

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setInverse(inv)
## Returns a matrix that is the inverse of 'x'
	inv
}
