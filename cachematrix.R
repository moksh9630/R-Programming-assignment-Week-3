## makeCacheMatrix returns a list which contains functions to set and get a matrix and its inverse
## cacheSolve function is passed the output of makeCacheMatrix which then calculates the inverse of matrix given.
## If the input value of matrix is not changed, it fetches the cached inverse.

## makeCacheMatrix function will create a matrix and set of functions to get the matrix and its inverse and will pass it to the parent environment

makeCacheMatrix <- function(x = matrix()) {
   inve <- NULL
   set <- function(y = matrix()) {
     x <<- y
     inve <<- NULL
   }
   get <- function() x
   setinverse <- function(solve) inve <<- solve
   getinverse <- function() inve
   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function will receive the list of functions passed from previous function and it will see if the inverse of given matrix is already present in cache.
## Otherwise it will calculate the inverse and return it
cacheSolve <- function(x, ...) {
            inve <- x$getinverse()
    if(!is.null(inve) && is.matrix(inve)) { 
      message("getting cached data")
      return(inve)
    }
  data <- x$get()
    inve <- solve(data, ...)
    x$setinverse(inve)
    inve
    }
