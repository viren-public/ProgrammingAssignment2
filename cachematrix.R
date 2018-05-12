## Caching the inverse matrix: Following functions are to cache 
## the inverse of a matrix

## This function creates a special "matrix" object that can cache 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinversem <- function(inverse) im <<- inverse
  getinversem <- function() im
  list(set = set, get = get,
       setinversem = setinversem,
       getinversem = getinversem)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the function will  
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  im <- x$getinversem()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinversem(im)
  im
}
