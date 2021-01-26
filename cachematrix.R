## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Caches the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  l <- NULL
  set <- function(y) {
    x <<- y
    l <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) l <<- inverse
  getinverse <- function() l
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function gets the inverse of the matrix from the cache or if its null solves it and stores it in cache
cacheSolve <- function(x, ...) {
  ## try to get from cache
  l <- x$getinverse()
  if (!is.null(l)) {
    return(l)
  }
  data <- x$get()
  #make inverse
  l <- solve(data, ...)
  #save in cache
  x$setinverse(l)
  l
}
