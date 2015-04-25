## Functions that will cache the inverse of a matrix and return the inverse 
## from cache if that was already calculated


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  ## Initialize the inversed object
  m <- NULL
  
  ## Method to set the matrix
  set <- function( matrix ) {
    x <<- matrix
    m <<- NULL
  }
  
  ## Method the get the matrix
  get <- function() x
  
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) m <<- inverse
  
  ## Method to get the inverse of the matrix
  getInverse <- function() m
  
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function computes the inverse of the special "matrix" 
## returned by  makeCacheMatrix  above. If the inverse has already been calculated
## (and the matrix has not changed), then  cacheSolve  should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        
  ## Get the inverse of 'x'
  m <- x$getInverse()
  
  ## Return the inverse from cache if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from input object
  data <- x$get()
  
  ## Calculate the inverse 
  m <- solve(data)
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
}
