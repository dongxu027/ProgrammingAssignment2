## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

  inv1 <- NULL
  ## define a set function.
  set <- function(y) {
    x <<- y
    inv1 <<- NULL
  }
  
  ## matrix inverse operation
  get <- function() x
  setinverse <- function() inv1 <<- solve(x)
  getinverse <- function() inv1
  
  ## create a list to store the matrix inverse.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
  
  ## get its inverse.
  inv1 <- x$getinverse()
  
  ## If the inverse has already been calculated, 
  ## retrieve the inverse from the cache.
  if(!is.null(inv1)) {
    message("getting cached data")
    return(inv1)
  }
  
  ## otherwise return the newly calculated inverse below.
  else{
    inv2<-solve(x)
    return(inv2)
  }

}
