##This function creates a special "matrix" object that can cache its inverse.
##For this function, input x was used as a matrix and the solved value "q" was used as a null value
##inverse was used to get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  q <- NULL
  set <- function(y){
    x <<- y
    q <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) q <<- inverse
  getInverse <- function() q 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  q <- x$getInverse()
  if(!is.null(q)){
    message("getting cached data")
    return(q)
  }
  mat <- x$get()
  q <- solve(mat,...)
  x$setInverse(q)
  q
}