
# The first function, `makeCacheMatrix` creates a "list", which is
# containing a function to

# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse of the matrix
# 4.  get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


# The function 'cacheSolve' returns the inverse of the matrix. It checks if
# the inverse has already been calculated If so, it gets the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse, 
# sets the value in the cache via 'setinv' function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Data from cache")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
