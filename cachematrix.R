##There are 2 functions makecacheMatrix() and cacheSolve().
##The idea behind these 2 functions is that one of the function(makecacheMatrix()) computes the inverse of a matrix.
##The other function caches the inverse of the matrix and returns the cached value when called by the other function or computes the inverse if not calculated already and then caches the value.

## The makeCacheMatrix() function accepts a matrix as input and returns a list with 4 functions that can be performed on the matrix.
##The 4 functions do the following operations
##1.Retrieve the current matrix value and also set new matrix values.
##2.Get the inverse of a current matrix or compute and set the values for inverse of a new matrix.

 

makeCacheMatrix <- function(x = matrix()) {m <- NULL
setmatrix <- function(y) {
x <<- y
    m <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = setmatrix, get = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse) }


## The cacheSolve() function takes the matrix input from makeCacheMatrix().
##The function computes and returns the inverse of the input matrix if the function returning the cached inverse value of the matrix is NULL
##else the function returns the cached value for the inverse of the Matrix.

cacheSolve <- function(x) {m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m }
