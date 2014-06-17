## Example usage with a square matrix:
## x<-matrix(c(3,2,4,16),2,2)
## y <- makeCacheMatrix(x)
## cacheSolve(y)

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	invMatrix <- NULL
	
	## set the given value and matrix outside of the current environment
	set <- function(y) {
		x <<- y
		invMatrix <<- NULL
	}
	
	## get the value of the matrix
	get <- function() x
	
	## set the inverse of the matrix
	setinv <- function(i) invMatrix <<- i
	## get the inverse of the matrix
	getinv <- function() invMatrix
	
	## list of functions to manipulate the matrix
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then
## cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
 		## get the inverse of the matrix
		invMatrix <- x$getinv()
		
		## check if there is the matrix
		## if available: print message and return chached matrix
		if(!is.null(invMatrix)) {
			print("getting cached data")
			return(invMatrix)
		}
		## get the inverse of the matrix
		data <- x$get()
		invMatrix <- solve(data, ...)
		
		## set the inverse of the matrix
		x$setInv(invMatrix)
		invMatrix
}
