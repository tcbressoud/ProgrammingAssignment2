## The following two functions work together to support maintaining
## a matrix and a cache version of its inverse.
## 
## Thopmas C. Bressoud

## The first function takes a matrix as an argument and defines
## setter and getter functions for accessing the matrix and
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## x (the parameter) and I (defined below) are defined in the environment 
  ## of the current function.
  I <- NULL
  
  ## Define a set of internal functions -- setters and getters for
  ## the variables in the environment of this "object", in this case M and I.
  set <- function(N) {
    x <<- N    # So this is setting the parameter M in the parent function's enviroment
    I <<- NULL # Whenever x changes, the cached inverse, I must reset
  }
  get <- function() x 
  ## Setter and getter for the inverse matrix
  setinv <- function(newinv) I <<- newinv
  getinv <- function() I
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The tandem function, cacheSolve, is an extended solve that, given a matrix,
## finds its inverse.  If a previous cacheSolve has already performed the inverse
## the cached version is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    #message("getting cached data")
    return(inv)
  }
  M <- x$get()
  inv <- solve(M, ...)
  x$setinv(inv)
  inv
}
