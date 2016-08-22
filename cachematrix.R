# To improve performance when doing matrix inversion it is wise to cache the inversion.
# Whenever inversion is needed first check the cache and take the result form thare when it is prsent
# This saves computation time.
# Below two functions are used to compute and cache the inverse of a matrix and perform the above described check. 

# The function makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(y) {
    mat <<- y
    inv <<- NULL
  }
  get <- function() mat
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. 
# First is checked whether the result is already in cache to avoid computing again.
# If it is it is immediately returned, otherwise the inverse is taken put into vcache and returned

cacheSolve <- function(mat, ...) {
  inv <- mat$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- mat$get()
  inv <- solve(data)
  mat$setinverse(inv)
  inv
}
