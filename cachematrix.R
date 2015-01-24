## The following two functions will utilize
## stored functions within lists and lexographical
## scoping to create a "pseudo-matrix" which is
## is capable of storing its inverse.

## This function makes a list that contains
## a matrix (x) and its inverse (y)
## (y) is only generated once setinverse
## is called.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y)
	{	x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This is the thingy that checks if the matrix is inverted
## If it is, it returns its stored inversion
## If it is not, it tells the matrix to invert itself.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m))
	{	message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
