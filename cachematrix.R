##The value of the inverse of the matrix is cached to avoid repetetive 
##calculation

##The function makeCacheMatrix creates a special matrix, which is a list 
##containing functions to set or get the values of the matrix and the inverse 
##of the matrix

makeCacheMatrix <- function (x=matrix()){
  inv <- NULL
  set <-function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function() inv <<- solve(x) #calculate the inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The function cacheSolve returns the inverse of the matrix created above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
