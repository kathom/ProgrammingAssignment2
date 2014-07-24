## This pair of functions caches the inverse of a square matrix.

## This function creates a list of functions that:
## 1) Set the value of the matrix
## 2) Get the value of the matrix
## 3) Set the value of the inverse
## 4) Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
      x <<- y 
      inv <<- NULL
  }
  get <- function() x 
  setinverse <- function(solve) inv <<-solve 
  getinverse <- function() inv 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function gives the inverse of the matrix specified in the above function.
## If it has already been computed, it gets the inverse from the cache.
## If it has not already been computed, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache via the setinverse funciton.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
