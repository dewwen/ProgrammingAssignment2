## Below are two functions that are used to create a spical object that stores a matrix and caches its inverse

## function makeCacheMatrix creates a special "matrix", which is a list containing functions to 
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse of the matrix
##  4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse_x <- NULL
    set <- function(y) {
    	x <<- y
        inverse_x <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse_x <<- inverse
    getinverse <- function() inverse_x

    ## create a list which includes the above four functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## function cacheSolve calculates the inverse of the matrix created with the above function. It first checks to see if the inverse of the matrix
## has already been calculated. If so, it use get() function to get the inverse from the cache and skips the computation. Otherwise, it calculates
## the inverse of the matrix and sets the value of the inverse in the cache via the setinverse() function

cacheSolve <- function(x, ...) {
    inverse_x <- x$getinverse()	
	## check if the inverse of the matrix has already been calculated
    if(!is.null(inverse_x)) 
    {
    	## inverse of the matrix already exists, so just return the cache
    	message("getting cached data")
    	return(inverse_x)
    }
    ## if not exists, calculate the inverse of 'x' below
    data <- x$get()
    inverse_x <- solve(data, ...)
    x$setinverse(inverse_x)
    ## Return a matrix that is the inverse of 'x'
    inverse_x
}
