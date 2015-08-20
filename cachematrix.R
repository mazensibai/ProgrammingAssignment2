## makeCacheMatrix & cacheSolve functions are used to cache potentially time-consuming computations (inverting) of Matrices


## This function, makeCacheMatrix creates a special "Matrix" which can:
## Set the value of Matrix
## Get the value of Matrix
## Set the value of inversion
## Get the value of inversion

makeCacheMatrix <- function(x = matrix()) {
 m <- matrix()
 set <- function(y) {
   x <<- y
  m <<- matrix()
 }
  get <- function() x
 setinverse <- function(inverse) m <<- inverse
 getinverse <- function() m
  list(set = set, get = get,
  setinverse = setinverse,
   getinverse = getinverse)

}


##cacheSolve:This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the 
##  inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		  m <- x$getinverse()
        if(!is.na(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}
