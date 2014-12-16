## Functions for creating and using inverted matrices which caching ability
##
## Usage example: 
##    > m <- matrix(c(1,0,5,2,1,6,3,4,0), nrow=3, ncol=3)
##    > cm <- makeCacheMatrix(m)
##    > cacheSolve(mcm)
##    ## Output the matrix inverse ##
##    > cacheSolve(mcm)
##    ## Output the matrix inverse and display the message 'getting cached data' ##


## The function makeCacheMatrix creates a matrix object that can cache 
##  its inverse by using scoping rules
makeCacheMatrix <- function(x = matrix()) {

    # Initialize the stored inverse value to NULL
    i <- NULL

    # Set the matrix values
    set <- function(y) {
        x <<- y
        # If the matrix changes the inverse must be set to NULL
        i <<- NULL
    }

    # Get matrix values
    get <- function() x

    # Set the matrix inverse 
    setinverse <- function(inverse) i <<- inverse

    # Return a list of the functions that can be used
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## The function cacheSolve computes the inverse of the matrix returned by
## makeCacheMatrix function above. 
## If the inverse has already been calculated then the cachesolve retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {

    # Grab the matrix inverse
    i <- x$getinverse()

    # If the inverse is cached then return it
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }

    # If not, then grab the matrix
    data <- x$get()

    # Compute the matrix inverse
    i <- solve(data, ...)

    # Store the inverse in the cache
    x$setinverse(i)

    # Return the matrix inverse
    i

}
