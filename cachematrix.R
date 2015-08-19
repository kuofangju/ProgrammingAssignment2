## Matrix inversion is usually costly computation 
## and there may be some benefit to 
## caching the inverse of a matrix that calculated before instead of 
## computing it repeatedly  

## This function create a special "matrix" object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL
	setmatrix <- function(y) {
		x <<- y
		inv <<- NULL 
	}
	getmatrix <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(setmatrix=setmatrix, getmatrix=getmatrix,
		setinv=setinv, getinv=getinv)
}


## This function copmute the inverse of the special "Matrix" 
## from above makeCacheMatrix function. 
## If the inverse has been calculated already, 
## then cacheSolve should get the inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinv()
		if(!is.null(inv)) {
			message("getting cached inverse")
			return(inv)
		}
		matrixdata <- x$getmatrix()
		inv <- solve(matrixdata, ...)
		x$setinv(inv)
		return(inv)

}
