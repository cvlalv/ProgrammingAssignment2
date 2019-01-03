## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##set the value of the matrix
##then get the value of the matrix
##set the value of the inverse
##get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  } #set the value of the matrix
  get <- function() x #get the value of the matrix
  setinverse <- function(solve) m <<- solve  #set the value of the inverse
  getinverse <- function() m  #get the value of the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




## Write a short comment describing this function
#first checks to see if the inverse of the matrix is already calculated. If so, it gets the inverse from the catche 
#Otherwise, it calculates the inverse of the matrix and sets the value of inverse
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  # first checks to see if the mean has already been calculated
  if(!is.null(m)) {
    message("TO cached data")
    return(m)
  }
  #If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
       
