## Put comments here that give an overall description of what your
## functions do

## The following functions are used to create a special object that stores a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinver <- function(inverse) i <<- inverse
  getinver <- function() i
  list(set = set,
       get = get,
       setinver = setinver,
       getinver = getinver)
}


## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinver()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinver(i)
  i
}
