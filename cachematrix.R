## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its mean
## The inverse is stored in the 'i' object
## the set function is used to set the matrix value
## get returns the matrix stored in the special "matrix" object
## setinverse is used to save the inverse in the cache
## getinverse returns the inverse matrix stored in the cache
makeCacheMatrix <- function(x = matrix()) {
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


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated, 
## then it retrieves the inverse from the cache.
## else it calculates the inverse and stores it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
