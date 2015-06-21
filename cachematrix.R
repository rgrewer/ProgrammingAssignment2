## These two functions are designed to create a matrix object 
## that has the ability to cache its own inverse, the 
## computation of which is usually time-consuming.

makeCacheMatrix <- function(x = matrix()) { ## Purpose: create a "vectorized" matrix X with the ability to cache its own inverse
  m <- NULL ## sets the inverse cache to empty by default (since it hasn't been calculated yet)
  set <- function(y) {
    x <<- y
    m <<- NULL ## ensures that the inverse cache is emptied when you set the matrix to something new
  } 
  
  get <- function() x ## returns the matrix
  setInverse <- function(Solve) m <<- Solve ## called during cacheSolve, does not do any solving itself
  getInverse <- function() m ## returns the inverse of the matrix
  
  ## The list command effectively specifies which of the above objects will be accessible outside the matrix object in the form x$get, etc.
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
}

cacheSolve <- function(x, ...) {        ## Purpose: Return a matrix that is the inverse of 'x'

  Inv <- x$getInverse() ## Pull in the data from the cache, if it exists.  Will return NULL if no data is stored
  if(!is.null(Inv)) { ## if it's not NULL, then send a message to the console and print the cached data
    message("getting cached data")
    return(Inv)  ## this also ends the function without doing any of the below calculations
  }
  data <- x$get()  ## Pull in the matrix itself, in preparation for finding the inverse
  Inv <- solve(data, ...) ## find the inverse
  x$setInverse(Inv) ## Store the inverse in cache
  Inv  ## report result of calculation
}