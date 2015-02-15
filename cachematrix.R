## This program creates a matrix and caches its inverse for retrieval

## Function 1 of 2, makeCacheMatrix, creates the matrix 'x', calculates its inverse,
## and caches the values

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function 2 of 2, cacheSolve, retrieves the inverse of matrix 'x' from the cache

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
