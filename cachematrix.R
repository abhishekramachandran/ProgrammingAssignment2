## makeCacheMatrix creates the object that can cache it's inverse.
## cacheSolve computes the inverse of the special matrix object 
## created by makeCacheMatrix.

## makeCacheMatrix takes an invertible matrix as input 
## and returns a list of functions (created in the execution 
## environment of this function call).
## The data and the inverses are cached in this execution environment.
## This is also the enclosing environment for the return functions.


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve checks if the inverse is already cached
## If not, they are calculated and cached using setinverse defined in makeCacheMatrix.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i ## Returns a matrix that is the inverse of 'x'
}
