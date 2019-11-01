## makeCachematrix and cachesolve work together to stores a matrix object, computes the inverse,
## and caches the matrix's inverse.

## makeCachematrix is a function that stores a matrix object, caches the inverse,
## builds four functions (set, get, setinverse, getinverse), 
## and returns it to the parent environment as a list   

makeCachematrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cachesolve pulls the matrixobject getinverse from makeCahematrix if the matrix has been solved before 
## if the matrix has not previously been inversed, cachesolve will retreieve get and use it to solve for the inverse.

cachesolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("retriving cached inverse matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
