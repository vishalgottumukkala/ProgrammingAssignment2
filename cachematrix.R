## An R function able to cache potentially time-consuming computations


## makeCacheMatrix creates a special "matrix" 
##that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
   inver <- NULL
   set <- function(y){
## assign x and inver to parent environment
## so that it can be used in cacheSolve
      x <<- y
      inver <<- NULL
   }
   get <- function()x
   setInverse <- function(inverse) inver <<- inverse
   getInverse <- function() inver 
   list(set = set, get = get, 
        setInverse = setInverse, 
        getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
## Returns the matrix that is the inverse of "x"    
   inver <- x$getInverse()
## If inverse has already been calculated
## cached data is retrieved
   if(!is.null(inver)){
      message("getting cached data")
      return(inver)
   }
## solves for the inverse of matrix
## if it has not been retrieved above
   ans <- x$get()
   inver <- solve(ans,...)
   x$setInverse(inver)
   inver
}