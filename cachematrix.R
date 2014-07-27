## 27/07/2014, psyentist 
## There are two functions in this file, the makeCacheMatrix and the cacheSolve,
## as requested in the assignment description.


## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.
## We assume that the argument of the function is given properly (as an NxN
## matrix) and that the matrix is invertable.

makeCacheMatrix <- function(x = matrix()) {
  mInv <- NULL
  set <- function(y) {
    x <<- y
    mInv <<- NULL
  }
  get <- function() x
  setmInv <- function() mInv <<- solve(x)
  getmInv <- function() mInv
  list(set = set, get = get,
       setmInv = setmInv,
       getmInv= getmInv)
}

## cacheSolve will calculate the inverse of the matrix. If the inverse was
## calculated earlier, it will use the cached data instead.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mInv <- x$getmInv()
  if(!is.null(mInv)) {
    message("getting cached data")
    return(mInv)
  }
  data <- x$get()
  mInv <- solve(data, ...)
  x$setmInv()
  mInv
}