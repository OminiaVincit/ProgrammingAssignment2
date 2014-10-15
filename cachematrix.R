##  Matrix inversion is usually a costly computation and there may be some 
##    benefit to cache the inverse of a matrix rather than 
##    compute it repeatedly
##  Assume that the matrix supplied is invertible

##   MAKECACHEMATRIX: creates a special "matrix" object that can
##     cache its inverse, return to
##       1. set the value of the matrix
##       2. get the value of the matrix
##       3. set the value of the inverse
##       4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(vx) {
    x <<- vx
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invmtr) inv <<- invmtr
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


##  CACHESOLVE: calculate the inverse of the matrix
##    If the inverse has already computed, return it without calculation
##    If not, compute the inverse, store in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  mtrx <- x$get()
  inv <- solve(mtrx)
  # Set inverse matrix to the cache
  x$setinv(inv)
  return(inv)
}

## Sample run:
##  > x <- matrix(c(1,2,3,4),nrow=2,ncol=2)
##  > m <- makeCacheMatrix(x)
##  > m$get()
##  [,1] [,2]
##  [1,]    1    3
##  [2,]    2    4
## > cacheSolve(m)
##  [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
##  > cacheSolve(m)
## getting cached data.
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## >