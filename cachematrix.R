## Caching the Inverse of a Matrix
##
## A pair of functions will cache the inverse of a matrix


## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL                     # Begins by set the inverse matrix to NULL
     set <- function(y) {           
          x <<- y                  # Defines a function to set the new matrix
          m <<- NULL               # and resets the inverse matrix
     }
     get <- function() x           # To return the input matrix
     setinverse <- function(solve) m <<- solve    # To set the inverse matrix
     getinverse <- function() m                   # To return the inverse matrix
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     
     ## Return a matrix that is the inverse of 'x'
     
     m <- x$getinverse()
     if(!is.null(m)) {                       # Checks to see if the inverse matrix has already been calculated.
          message("getting cached data")     # If so, it gets the inverse matrix from the cache 
          return(m)                          # and skips the computation.
     }
     data <- x$get()
     m <- solve(data, ...)                   # Compute for the inverse matrix
     x$setinverse(m)                         # Sets the inverse matrix in the cache 
     m     
}