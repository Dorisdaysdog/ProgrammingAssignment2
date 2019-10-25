# These two functions work together to check if a supplied matrix already has an inverse calculated
#   if it does then it is just retrieved from the cache object and if not it is colculated using solve()
# use by running: aMatrixObject <- makeCacheMatrix() to make the special matrix object
# then use this objects set method to put your matrix of interest (c) in like so: aMatrixObject$set(c)
# and can then calc the inverse the first time using cacheSolve(aMatrixObject)
# any other attempts to get the inverse of this metrix will pull it from the cache


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.  
#  It has getter and setter methods that are part of the new objects environment and 
#   these allow other functions (eg cacheSolve) to access variables from this parent environment (x and m in this case)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  print(x,m)
  set <- function(y) {
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


##  cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#    If the inverse has already been calculated (and the matrix has not changed), 
#    then the cachesolve should retrieve the inverse from the cache.

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
  m
}
