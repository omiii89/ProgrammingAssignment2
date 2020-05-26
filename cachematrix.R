## To simply put, we will be caching the inverse 
## of a "matrix"

## This function creates a "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## It will compute the inverse "matrix" given by 
## above function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## To test the above code, create a matrix and
## provide it as an argument to makeCachematrix()
## function and the output matrix of that should
## be given to the cacheSolve() function.

## Happy Coding :)
