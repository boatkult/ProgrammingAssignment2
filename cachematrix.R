## Series of two functions that return the inverse of a matrix,
## either by calculating with solve() or, if it's already been
## calculated, by retrieving the cached object.

## First function takes a matrix ('x') as an argument, and outputs
## a list of set and get functions to assign and return values
## for 'x' and its inverse, 'inv'. Set functions assign values for
## 'x' and 'inv' in the parent environment in order to cache them.
## If this function is used to set the value of 'x', the value of
## 'inv' is set to NULL (i.e., the cache is cleared).

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invmat) inv <<- invmat
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv,
       getinv = getinv)
}


## Second function takes the previous function's output
## (i.e., a list of get and set functions) as an argument.
## First uses a get function to retrieve the value for 'inv' from
## cache and determine whether it's NULL. If it's not NULL, it
## returns the value for 'inv'. If it is NULL, the inverse matrix
## is calculated using solve(), and assigned to 'inv'.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}



