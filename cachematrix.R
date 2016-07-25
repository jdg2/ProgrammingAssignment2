## these functions allow a user to save time on getting the inverse
## of a matrix by using a cache to return the inverse... 
## if the matrix has already been solved

## this first function takes a matrix input
## and outputs a list of functions to be used in cacheSolve
## submit in form "cacheSolve(makeCacheMatrix(matrix))"

makeCacheMatrix <- function(x = matrix()) {
  mtrx_inv <<- NULL
  set <- function(y) {
    x<<- y
    mtrx_inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) mtrx_inv <<- solve 
  getinv <- function() mtrx_inv
  list(set = set, get = get, setinv = setinv, getinv = getinv) ##named functions
}

## This function tests so see if the cach is null
## if the cache is not null it returns the cache
## if the cache is not null it returns the cached value

cacheSolve <- function(x, ...) {
  mtrx_inv <- x$getinv()       ##calls the inverse from makeCacheMatrix
  if(!is.null(mtrx_inv)) {    ## checks to see if it is not null
    message("hold on, gettin the inverse")
    return(mtrx_inv)     ## returns the matrix if it not null
  }
  data<-x$get()              ## returns that matrix
  mtrx_inv<- solve(data, ...) ## solve the matrix if it is null
  x$setinv(mtrx_inv)     ## cache the inverse 
  mtrx_inv               ## call the inverse for displat
}
