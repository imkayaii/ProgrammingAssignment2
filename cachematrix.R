## This is a pair of functions that cache the inverse of a matrix, assuming that the matrix supplied is always invertible


## makeCacheMatrix() is a function that creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inver <- NULL
	set <- function(y) {
		x <<- y
		inver <<- NULL
	}
	get <- function() x
	setinversion <- function(inversion) inver <<- inversion
	getinversion <- function() inver
	list(set = set, get = get, setinversion = setinversion, getinversion = getinversion)

}



## cacheSolve() is a function that computes the inverse of the special "matrix" returned by makeCacheMatrix()
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
	inver <- x$getinversion()
	if (!is.null(inver)) {
		message("getting cached data")
		return(inver)
	}
	data <- x$get()
	inver <- solve(data, ...)
	x$setinversion(inver)
	inver
}
