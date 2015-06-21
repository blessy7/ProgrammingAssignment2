## To avoid repeated computations of the inverse of a matrix and for faster
## calculation of inverse of a matrix we create a special matrix within a
## different scope other than the current scope to store the computed value
## through makeCacheMatrix().
## When the inverse of a matrix is to be computed the cache is checked first
## and if the value is present, then the cached value is returned else
## inverse is calculated and stored in the cache through cacheSolve()


## makeCacheMatrix creates a special matrix to cache the inverse 
## of a given matrix

makeCacheMatrix <- function(x = matrix()) {
	
	i <- NULL
	set <- function(y){
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	
	list(set = set, get=get, 
		setinverse = setinverse, 
		getinverse = getinverse)

}


## cacheSolve function checks whether the inverse of the matrix exists.
## If it does exist then it returns the cached inverse
## else it computes the inverse and caches it through setinverse()

cacheSolve <- function(x, ...) {

	i <- x$getinverse()
    ## inverse is present in cache
	if(!is.null(i)){
		message("getting cached data")
		return(i)
	}
   ## inverse not present in cache hence calculate and store in cache
	data <- x$get()
	i <- solve(data,...)
	x$setinverse(i)
	i


}
