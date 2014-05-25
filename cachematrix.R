## makeCacheMatrix creates a special matrix object
## calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, 
## it will instead find it in the cache and return it
## and not calculate it again.
 
makeCacheMatrix <- function(x = matrix()) {
    inverseOf_x <- NULL
    setter <- function(y) {
        x <<- y
        inverseOf_x <<- NULL
    }
    getter <- function() x
    setinverse<- function(inverse) inverseOf_x <<-inverse
    getinverse <- function() inverseOf_x
    list(setter = setter, getter = getter,
         setinverse = setinverse,
         getinverse = getinverse)
}
 
## The function cacheSolve returns the inverse of a matrix 
## created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it
## if not, it computes, caches, and returns it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseOf_x <- x$getinverse()
    if (!is.null(inverseOf_x)) {
        message("retrieving the cached inverse matrix")
        return(inverseOf_x)
    } else {
        inverseOf_x <- solve(x$get())
        x$setinverse(inverseOf_x)
        return(inverseOf_x)
    }
}