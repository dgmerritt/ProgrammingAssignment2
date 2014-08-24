## makeCacheMatrix() and cacheSolve()
# provide a framework for generating
# the inverse of an input matrix.
# That is, if Z is a square matrix input to makeCacheMatrix(),
# and invZ is its inverse output from cacheSource(), 
# Z %*% invZ will return an identity matrix.
# If the inverse matrix has been generated previously in the session,
# cacheSource() will retrieve the cached value.


# function makeCacheMatrix():
# Takes input matrix and returns a list of internal
# functions to get and set the inverse matrix in cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    # x <<- y 
    # searches in parent environment 
    # for existing definition of x
    # before assigning y to x
    x <<- y
    #clears cached m
    m <<- NULL
  }
  # get function returns x (takes no argument)
  get <- function() x
  setmatinv <- function(solve) {m <<- solve}
  getmatinv <- function() m
  
  list(set = set, 
    get = get,
    setmatinv = setmatinv,
    getmatinv = getmatinv)
   
}

## function cacheSolve():
# Takes list output of makeCacheMatrix()
# and returns matrix which is inverse of 
# the matrix input to makeCacheMatrix().
# If inverse matrix has already been 
# calculated, returns cached value.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatinv()
  if (!is.null(m)){
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatinv(m)
  m
}