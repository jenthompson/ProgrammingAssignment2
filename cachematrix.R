## Program Assignment 2
## Jennifer Thompson

## The following two functions compute the inverse of a matrix
## then caches the inverse so that it will not be computed repeatedly

## The makeCacheMatrix function creates an object to cache the inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(inv) m <<- inv
      getinv <- function() m
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function computes the matrix inverse 
## or retrieves the cached object if it exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if(!is.null(m)) {
          message("getting cached data")
          return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}
