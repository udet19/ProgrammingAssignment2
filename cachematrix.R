## Put comments here that give an overall description of what your
## functions do


## This function is a constructor function that creates a list of
## four function to create and handle the matrix

makeCacheMatrix <- function(x = matrix()) {
  ## check if the matrix in the square form
  ## if not, execution will be aborted
  if (dim(x)[1]==dim(x)[2]) { 
  
  i <-NULL
  ## set matrix
  ## parameters:  data as numeric vector "foldable" to square matrix
  ##              y as numeric value for square matrix dimension
  set <- function (y) {
    
    x <<- y
    i <<- NULL
  
  }
  ## get returns the matrix
  get <- function () x
  
  ## setinvert calculates the invert values of the matrix
  setinvert <- function(solve) i<<-solve
  
  ## getinvert returns the invert of the matrix
  getinvert <- function() i
  list(set = set, get = get, setinvert = setinvert, getinvert = getinvert)
  }
  else {
    message("Matrix has to be in square form! Execution aborted")
  }
}


## The function caches the invert of a matrix to preserve the resources

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinvert()
  if (!is.null(i)) {
    message("retrieving cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinvert(i)
  i
}

