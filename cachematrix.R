## The following functions will create a matrix and cache the inverse of matrix 
## This allows for a recall of the matrix instead of recomputing.

## Create function makeCacheMatrix containing list to set and get matrix
## set and get inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
        x <<- y
        m <<- NULL
  }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Create cacheSolve to calculate the inverse of the matrix

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse
        
       ## Check if inverse is already calculated
        
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
