## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here).
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    # Initially set to NULL
    # Changes when the user sets the value
    inv <- NULL

    # set function
    # Sets the matrix itself but not the inverse
    set <- function(y) {
        x <<- y    # Set the value
        m <<- NULL # Clear the cache
    }
    # Define function to get the value of the matrix
    get <- function() x
    # Define function to set the inverse. This is only used by getinverse() when
    # there is no cached inverse
    setInverse <- function(inverse) inv <<- inverse
    # Define function to get the inverse
    getInverse <- function() inv
    
    # Return a list with the above four functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve retrieves the 
# inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # Get the current state of the inverse and see if it
        # has been computed yet
        inv <- x$getInverse()
        
        if(!is.null(inv)){
              message("Getting cached matrix")
              return(inv)  
        }
        # The cache was empty. We need to calculate it, cache it, and then return it.
        data <- x$get()

        # Find the inverse
        inv <- solve(data, ...)

        # Cache this result in the object
        x$setInverse(inv)

        # Return this new result
        inv 
}

## Sample run:
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## No cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > 
