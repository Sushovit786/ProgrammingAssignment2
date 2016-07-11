## Put comments here that give an overall description of what your
## functions do
## creates, stores and recall a matrix and its inverse
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	##creates a custom matrix 
	## stores the matrix in cache, get recalls the matrix
	m <- NULL
	set <- function(y) {
		x <<- y 
		m <<- NULL
	}
	
	get <- function() x
	setInverse <- function(solve) m<<- solve
	getInverse <- function() m 
	list(set = set, get = get,
			setInverse = setInverse,
			getInverse = getInverse)
 }


## Write a short comment describing this function
## cacheSolve takes a custom matrix type created by the makecachematrix and calculates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
        	message("getting cached data")
        	return(m)
        	
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        
}
