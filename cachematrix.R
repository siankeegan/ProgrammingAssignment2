## Creating functions that solve the inverse of a matrix,
## cashes the result and returns the value from the cashe if 
## it has already been calculated in the environment.

## Creating a function the makes a matrix object and can cashe its
## inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y 
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list (set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}

## Creating a function that computes the inverse of the special 
## matrix returned by makeCasheMatrix above. If the inverse has 
## already been calculated (if the matrix has not been changed), 
## then the cashesolve should retrieve the inverse from the cashe.
cashesolve <- function(x=matrix(), ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cashed data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}

## Creating a matrix to use in the functions:
m <- makeCacheMatrix(matrix(5:8, nrow = 2, ncol = 2))

## Using the cashsolve function on the above matrix:
cashesolve(m)

