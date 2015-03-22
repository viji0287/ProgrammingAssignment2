## makeCacheMatrix has set of 4 functions inside it
## setmatrix function sets the matrix values
## getmatrix function returns the matrix that was set using setmatrix function
## setmatrixinverse computes the inverse of the matrix set using setmatrix
## getmatrixinverse return the inverse of the matrix set using setmatrixinverse


## makeCacheMatrix returns the list of four functions- setmatrix,getmatrix,
## setmatrixinverse, getmatrixinverse
makeCacheMatrix <- function(x = matrix()) {
  ## x is empty matrix
  m <- NULL
  setmatrix<- function(y) {
    ## input matrixy is assigned to x
    x <<- y
    m <<- NULL
  }
  getmatrix <- function() x
  setmatrixinverse <- function(solve) m <<- solve
  getmatrixinverse <- function() m
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setmatrixinverse = setmatrixinverse,
       getmatrixinverse = getmatrixinverse)

}


## cacheSolve returns the inverse of matrix 'x'
## it checks if the inverse is already computed and available in cache first
## if available in cache, it retrieve the value stored in cache and returns the same
## if not available in cache, it computes the inverse and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrixinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getmatrix()
  m <- solve(data, ...)
  x$setmatrixinverse(m)
  m
}
