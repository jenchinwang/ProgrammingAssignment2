## When below functions are executed in sequence, the user can get an inversed
## matrix of your input matrix with a message to know if the inversed
## matrix is already in cache!
 
## The first function will create a list of functions that will give
## the second function instruction what to do 

makeCacheMatrix <- function(x = matrix()) {
  inverse_x <- NULL
  set <- function(y) {
    x <<- y
    inverse_x <<- NULL
  }
  get <- function() x
  setsolve <- function(inverse_matrix) inverse_x <<- inverse_matrix
  getsolve <- function() inverse_x
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The second function will execute the computation to get an 
## inversed matrix of a given matrix

cacheSolve <- function(x, ...) {
  inverse_x <- x$getsolve()
  ## Print a message if data is already in the cache
  if(!is.null(inverse_x)) {
    message("getting cached data")
    return(inverse_x)
  }
  data <- x$get()
  inverse_x <- solve(data, ...)
  x$setsolve(inverse_x)
  ## Return a matrix that is the inverse of 'x'
  inverse_x
}
