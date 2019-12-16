## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix creates a list which contains a function to do the following: 
  ## 1. Set the value of the matrix   
  ## 2. Get the value of the matrix 
  ## 3. Set the value of the inverse of the matrix 
  ## 4. Get the value of the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {                            # Function that sets the value of the matrix
      x <<- y
      m <<- NULL
    }
    get <- function()x                              # Function that gets the value of the matrix
    setinverse <- function(solve) m <<- solve   # Function that sets the inverse of the matrix
    getinverse <- function() m                      # Function that gets the inverse of the matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function takes the inverse of the matrix created in the function above. 
## It first checks if the inverse is already defined, and if it is then it directly gets the inverse from the cache without
## having to perform the operation. If not, it computes the inverse and sets its value in the cache using the setinverse function. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m                                                 # m is the inverse of the matrix x
}
