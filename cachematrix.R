## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    matrixInverse<- NULL
    set <- function(y) {
        x <<- y
        matrixInverse <<- NULL
    }
    get <- function(){
        x
        
    }
    setInverse <- function(minverse){
        matrixInverse <<- minverse
    }
    getInverse <- function() {
        matrixInverse
    }
    list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrixInverse <- x$getInverse()
        if(!is.null(matrixInverse)) {
            message("getting cached data")
            return(matrixInverse)
        }
        data <- x$get()
        matrixInverse <- solve(data, ...)
        x$setInverse(matrixInverse)
        matrixInverse
}
