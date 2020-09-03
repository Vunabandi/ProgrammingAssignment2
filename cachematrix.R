## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix catches the inverse of a matrix with the object linv
makeCacheMatrix <- function(x = matrix(sample(1:124,25),5,5)) {
  linv <- NULL
  set <- function(y) {
    x <<- y
    linv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) linv <<- solve
  getsolve <- function() linv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
## Write a short comment describing this function
## when makeCacheMatrix does not work, we can rely on cacheSolve which does the same thing
cacheSolve <- function(x, ...) {
  linverse <- x$getsolve()
  if(!is.null(linverse)) {
    message("getting inversed matrix")
    return(linverse)
  }
  data <- x$get()
  linverse <- solve(data, ...)
  x$setsolve(linverse)
  linverse
}
