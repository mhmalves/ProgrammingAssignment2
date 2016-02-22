##
# Assignment: Caching the Inverse of a Matrix
#
# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than compute it repeatedly.
#
# Your assignment is to write a pair of functions that cache the inverse of
# a matrix.
##

## 
# makeCacheMatrix(x = matrix()) function creates a special "matrix" object that
# can cache its inverse.
#
# Returns a list() containing a function to:
#    1. set the value of the matrix
#    2. get the value of the matrix
#    3. set the value of inverse of the matrix
#    4. get the value of inverse of the matrix
##
makeCacheMatrix <- function(x = matrix()) {
        
        # Variable to store the inverse matrix
        inv_Matrix <- NULL
        
        # Function to set the value of the matrix
        set <- function(y) {
                x <<- y
                
                # Overwriting the old value of the inverse matrix variable
                inv_Matrix <<- NULL
        }
        
        # Function to get the value of the matrix
        get <- function() x
        
        # Function to set the value of inverse of the matrix
        # with the solve() function
        setinverse <- function(solve) inv_Matrix <<- solve
        
        # Function to get the value of inverse of the matrix
        getinverse <- function() inv_Matrix
        
        # The returning list()
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## 
# cacheSolve(x, ...) function computes the inverse of the special "matrix"
# returned by makeCacheMatrix() above.
#
# If the inverse has already been calculated (and the matrix has not changed),
# then the cacheSolve() should retrieve the inverse from the cache.
##
cacheSolve <- function(x, ...) {
        
        # Gets the inverse of the special "matrix"
        inv_Matrix <- x$getinverse()
        
        # If inv_Matrix is not null, returns inv_Matrix from cache
        if (!is.null(inv_Matrix)) {
                
                message("Getting value from cache")
                return(inv_Matrix)
        }
        
        # Retrieves the special "matrix" 
        matrix_M <- x$get()
        
        # Calculates the inverse matrix
        inv_Matrix <- solve(matrix_M)
        
        # Sets the inv_Matrix value at makeCacheMatrix environment
        x$setinverse(inv_Matrix)
        
        # Returns the inverse of the matrix
        inv_Matrix
}
