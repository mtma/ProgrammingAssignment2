
## This function creates a special matrix, which is a list containing a function to set and get a matrix and ist inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function calculates the inverse of the special matrix. It first check if the inverse has been already calculated, if so, it gets the inverse of the cache. Otherwise, it calculates the inverse of the data and sets it in the cache.


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}




