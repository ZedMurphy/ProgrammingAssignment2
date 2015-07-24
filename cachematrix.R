## These are two functions to compute and cache the inverse of a matrix


## Creating a matrix object that caches its inverse, the function 
## returns a list of values related to the matrix object

makeCacheMatrix <- function(mx = matrix()) {
  inverse <- NULL
  set <- function(x) {
    mx <<- x;
    inverse <<- NULL;
  }
  get <- function() return(mx);
  setinverse <- function(inv) inverse <<- inv;
  getinverse <- function() return(inverse);
  return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}


## Computing the inverse of the matrix object returned from the first 
## function (makeCacheMatrix)
## To prevent the function from computing the inverse from the very 
## same matrix again, it is told to get the cached value instead.

cacheSolve <- function(mx, ...) {
  inverse <- mx$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- mx$get()
  inverse <- solve(data, ...)
  mx$setinverse(inverse)
  return(inverse)
}
