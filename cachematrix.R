## This function creates a special "matrix" object that can cache its inverse
## Following the example with vector we
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## 1. Checks if inverse was already calculated and return calculated value
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    m
  } else {
  ## 2. Calculate the inverse and write to cache
    data <- x$get()
    ## If called with one matrix parameter, solve will return the inverse of a.
    m <- solve(data)
    x$setinverse(m)
    m  
  }
}
