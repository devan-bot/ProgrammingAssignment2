## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
    # Initialize the inverse property
    inv <- NULL
    
    # Method to set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Method to get the matrix
    get <- function() x
    
    # Method to set the inverse of the matrix
    setInverse <- function(inverse) inv <<- inverse
    
    # Method to get the inverse of the matrix
    getInverse <- function() inv
    
    # Return a list of the methods
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
    # Get the current inverse
    inv <- x$getInverse()
    
    # Return the inverse if it's already set (cached)
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Get the matrix from the object
    mat <- x$get()
    
    # Compute the inverse
    inv <- solve(mat, ...)
    
    # Set the inverse to the object
    x$setInverse(inv)
    
    # Return the inverse matrix
    inv
}
