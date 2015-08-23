## Put comments here that give an overall description of what your
## functions do
## these functions take a mtrix from user find its inverse and cache its value so
## that if the inverse is needed at later stage it can just be taken from cache and
## need not be calculated again

## Write a short comment describing this function
## this functions takes a matrix from the user and makes a
## list based upon certain fuctions performed on the matrix
## i_e "set" user can set the matrix whose inverse is to be cached by this command
## "get" gets the matrix that was previous set and prints it 
## "setinverse" sets the inverse as told by the user...if solve(x) given as an
## argument it finds the inverse and caches its value
## "getinverse" gets the inverse as found by setinverse command and prints it

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y = matrix()) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function
## this fuctions takes list from makeCacheMatrix function as an argument
## checks if the inverse of the matrix is cached.
## If true, value printed after informing the user that it got the 
## cached value otherwise it caculates the inverse and prints it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    ## Return a matrix that is the inverse of 'x' and was cached
    return(m)
  }
  data <- x$get()
  ## calculating inverse
  m <- solve(data, ...)
  x$setinverse(m)
  ## Return a matrix that is the inverse of 'x'
  m
  
}
