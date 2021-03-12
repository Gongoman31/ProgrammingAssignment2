## This program will create a matrix, then compute and return that matrix's inverse.

## The makeCacheMatrix function below creates a "special" matrix object that
## can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) 
    j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## The cacheSolve function below will compute and retrieve the inverse of the special matrix created above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    j <- x$getInverse()
    if(!is.null(j)){
      message("getting cached data")
      return(j)
    }
    mat <- x$get()
    ## The matrix that was created is invertible. The solve function below will compute the inverted matrix, and store
    ## the inverse into the object 'j.'
    j <- solve(mat,...)
    x$setInverse(j)
    j
  }
