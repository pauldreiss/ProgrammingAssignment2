## makeCacheMatrix creates a special "matrix", which is really a matrix
## containing a function to cache its inverse
## The function has other functions to:

## 1. 'set' the value of the matrix
## 2. 'get' the non-inverted value 'x' of the matrix
## 3. 'setmatrix' the value of the inverse of 'x' using 'solve'
## 4. 'getmatrix' the inverted value of 'x' as 'm'

## To test, first create a square invertible matrix (example from ?solve)
## hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
## h8 <- hilbert(8)
## Second, create a 'makeCacheMatrix object and passing your matrix
## Third, create a 'cacheSolve object passing your 'makeCachedMatrix object
## compare 'X' to your 'cacheSolve object for inverse
## try computing the inverse again using the same 'cacheSolve' object above to confirm
## the cached value is sent to the console and not re-calculated

## Each time you create a 'makeCacheMatrix' object, 'm' is set to NULL and must be re-calculated
## Each subsequent cacheSolve function call uses the cached value of 'm'

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Return a matrix that is the inverse of 'x' when makeCacheMatrix was created
## set 'm' to the cached calculated inverted value of 'x'
## If the inverse of 'x' has already been calculated then return(m), the cached inverted value of 'x'
## else, x$get the non-inverted matrix 'x' as 'data'
## 'solve(data)' for the inverse of the original non-inverted matrix 'x' as 'm'
## and print the value 'm' to the console
cacheSolve <- function(x, ...) {
  
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m  
}
