## The below functions enable the user to store inverse 
## of a given matrix in cache, so that this time-consuming computation
## can be sometimes omitted
## The project include 2 functions: makeCacheMatrix and cacheSolve


## The function makeCacheMatrix creates a list of functions:
  # set - sets the matrix
  # get - gets the matrix
  # setinverse - sets the inverse matrix
  # getinverse - gets the inverse matrix
## The function has one argument, which has to be an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function cacheSolves gives an inverse of an inveritble matrix, 
## it first checks if the inverse ewas calculated before
## The argument of a function is a list made by makeCacheMatrix

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
