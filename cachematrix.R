## The following code calculates the inverse of a given matrix as input argument ...
## to makeCacheMatrix() function. Then cachesolve() can be used to either calculate ...
## the inteverse of the matrix or return the cashed value of the inverse from memory

## makeCacheMatrix() can get a metrix as an input argument and assign ...
## memory location to the inverse and the matrix itself.
## memory location can be display by just caling the faunction without assignment.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmat <- function(solve) m <<- solve
  getmat <- function() m
  list(set = set, get = get,
       setmat = setmat,
       getmat = getmat)
}

## cachesolve() get the output of makeCacheMatrix() which are memory locations ...
## with or without the values of inverse. If there is no inverse ...
## calculated (i.e. the inverse is null and memory location is empty) it will ...
## be calculated otherwise the cashed value from memory will be returen.

cachesolve <- function(x, ...) {
  
  m <- x$getmat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmat(m)
  m  
}
