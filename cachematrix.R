## This file contains two function used in compute the inverse of matrix 'x'.
## makeCacheMatrix creates a list of supporting functions that support the setting/solving
## cachSolve returns ths inverse of matrix 'X' . Its caches the input/output so as not recompute if input matrix is solved and not changed.

# Creates a list of functions to store and solve the inverse of matrix.
# set - Sets the value of the input matrix
# get      - Gets the value of the input matrix
# setsolve - Sets the solved matrix for the given input.
# getsolve - Gets the solved matrix that is the inverse of get
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


##This function return a matrix that is the inverse of 'x'
# it first checks to see if there is already a solution computed and if the input matrix is identical to 
# the matrix that was used to calculate the solved matrix in the cache.
# The function assumes that the input matrix is inversable.
cacheSolve <- function(x, mat) {
  s <- x$getsolve()
  
  #check to see if the inverse matrix has been solved. 
  #It also check to see if the input matrix has changed or not
  if((!is.null(s)) && identical(x$get(), mat)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data)
  x$setsolve(s)
  s
}


# usage code showing the function in action.
#mat <- rbind(c(1,4),c(1,1))
#s <- makeCacheMatrix(mat)
# calls solve the first time
#cacheSolve(s, mat) 
# mat as not changed so solve cache is used instead.
#cacheSolve(s, mat)




