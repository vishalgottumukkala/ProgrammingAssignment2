##explain code in a few words
##explain makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y){
##explain <<-
      x <<- y
      inv <<- NULL
   }
   get <- function()x
   setInverse <- function(inverse) inv <<- inverse
   getInverse <- function() inv 
   list(set = set, get = get, 
        setInverse = setInverse, 
        getInverse = getInverse)
}

##explain cacheSolve

cacheSolve <- function(x, ...) {
   ## explain if statement
   inv <- x$getInverse()
   if(!is.null(inv)){
      message("getting cached data")
      return(inv)
   }
   ans <- x$get()
   inv <- solve(ans,...)
   x$setInverse(inv)
   inv
}