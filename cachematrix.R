## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## function first clears the existing Inverse m and returns the matrix m
## this function has subfunctions that can return inverse

makeCacheMatrix <- function(x = matrix()) {
# check it is a square matrix, if not make new
       m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
 
         list(set = set, get = get,
          setinverse = setinverse, 
          getinverse = getinverse)
}


## Write a short comment describing this function
## first get the inverse from above function
## and if the inverse is not null, then return the matrix from cache
## if the is null, then generate one and return m

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
