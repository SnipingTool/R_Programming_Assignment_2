## makeCacheMatrix creates a special 'matrix' i.e a list that contains functions to
## 1. set the values of entries in the matrix
## 2. get the values of entries in the matrix
## 1. set the inverse of the matrix
## 1. get the inverse of the matrix

## Creates the special 'matrix' by associating the above mentioned functions with it.
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Computes the inverse of the matrix if it is not already calculated,
## otherwise it obtains it from the cache.
cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("Getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      return(inv) 
}