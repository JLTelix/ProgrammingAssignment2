## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function is an update of the function makeVector.
## It make a list where will be saved the original matrix, the inverse (if it exists).

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## This function is an update of the function cacheVector.
## 1.- search if the inverse matrix has been calculated. If so the cache gets and ends
## 2.- Calculate the inverse matrix with the function solve(). 

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
