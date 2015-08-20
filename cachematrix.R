## Put comments here that give an overall description of what your
## functions do

## This function, makeVector creates a special "vector"

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


##Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix

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
