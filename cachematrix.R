## Put comments here that give an overall description of what your
## functions do
##The first functions creates a special matrix whose inverse can be cached and 
## the second function calculates the inverse of the matrix created by the first
## function or returned the inverse if already calculated.

## Write a short comment describing this function
##The makeCacheMatrix creates a matix that  can  cached its inverse

makeCacheMatrix <- function(x = matrix()) {
  original_mat <- NULL
  set <- function(y) {
    x <<- y
    original_mat <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) original_mat <<- solve
  getinverse <- function() original_mat
  list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function
## The cacheSolve function calculates the inverse of a matrix if not already 
##calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_mat <- x$getinverse()
  if(!is.null(inverse_mat)){
    message("Getting cached data")
    return(inverse_mat)
  }
  data <- x$get()
  inverse_mat <- solve(data, ...)
  x$setinverse(inverse_mat)
  inverse_mat
}
##test the function
m <- matrix(1:4, 2, 2)

cacheSolve(makeCacheMatrix(m))

