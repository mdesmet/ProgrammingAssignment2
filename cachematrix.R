## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly.
## These functions will help you do so.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	im <- NULL
	set <- function(y) {
			x <<- y
			im <<- NULL
	}
	get <- function() x
	setInversedMatrix <- function(inversedMatrix) im <<- inversedMatrix
	getInversedMatrix <- function() im
	list(set = set, get = get,
		 setInversedMatrix = setInversedMatrix,
		 getInversedMatrix = getInversedMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve shall retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		im <- x$getInversedMatrix()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setInversedMatrix(im)
        im
}
