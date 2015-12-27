## The following functions will cache the inverse of a matrix 
## so that it doesn't have to be computed repeatedly

## this function will creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y){
            x <<- y
            m <<- NULL
      }
      get <- function ()x
      setinverse <- function(solve)m <<- solve
      getinverse <- function()m
      list(get=get, set=set, setinverse=setinverse, getinverse=getinverse)

}


## this function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above.  IF the inverse has already been calculated, then the cacheSolveMatrix
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
      
}
