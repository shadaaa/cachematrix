## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## creating makeCacheMatrix to cache the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y ## #cache "x" matrix in the variable "y" outside current work environnment
    inv <<- NULL #cache the values in "inv" outside current work environment. 
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Write a short comment describing this function
##computes inverse of a special matrix
##if inverse have already computed, then it will skip computation.
##If not, it computes the inverse, sets the value in the cache via
## setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
