## Functions compute the inverse of a square matrix and store
## in cache allowing value to be retrieved quickly in the future
## without having to recalculate

## This function creates the four functions that will be used
## to set the matrix as well a set/hold the inverse once calculated

makeCacheMatrix <- function(x = matrix()) {

     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) m <<- inverse
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
     
}


## This function checks to see if inverse has been cached
## and if so uses the cache versus else recalculates

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data)
     x$setinverse(m)
     m
}
