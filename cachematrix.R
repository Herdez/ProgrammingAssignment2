## Functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
      
      ## Initialize inverse of X
      invx <- NULL
      ## Set the matrix
      set <- function( m ) {
            x <<- m
            invx <<- NULL
      }
      ## Get the matrix
      get <- function() {
            ## Return matrix
            x
      }
      ## Set the inverse of the matrix
      setInverse <- function(inverse) {
            invx <<- inverse
      }
      ## Get the inverse of the matrix
      getInverse <- function() {
            ## Inverse x
            invx
      }
      ## Return list of the methods
      list(set = set, get = get, 
           setInverse = setInverse, 
           getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
      ## Matrix that is the inverse of x
      invx <- x$getInverse()
      ## Return inverse if its already set
      if( !is.null(invx) ) {
            message("getting cached data")
            return(invx)
      }else{
            ## Get matrix 
            data <- x$get()
            ## Calculate inverse 
            invx <- solve(data) %*% data
            ## Set the inverse 
            x$setInverse(invx)
            ## Return the matrix
            return(invx)
      }
}

