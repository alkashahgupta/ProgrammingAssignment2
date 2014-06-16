##assume that the matrix supplied is always invertible.

## This first function create a special "matrix", by the help four functions
#set: set the value of the matrix
#get: get the value of the matrix
#setInverse : set the value of the inverse of a matrix
#getInverse : get the value of the inverse of a matrix
#finally it returns all these four functions


makeCacheMatrix <- function(x = matrix()) {
    #initializing an empty matrixInverse object to store the cached inverse
    matrixInverse<- NULL
    # This set function is setting value of matrix to the object that is defined in different environment (here its parent environment)
    set <- function(y) {
        x <<- y
        matrixInverse <<- NULL
    }
    
    # This get function is returning the value of x object to the called object
    get <- function(){
        x
        
    }
    #This setInverse function is setting the value of matrixInverse defined in another environment.
    setInverse <- function(minverse){
        matrixInverse <<- minverse
    }
    # This function is simply returning the value of object matrixInverse
    getInverse <- function() {
        matrixInverse
    }
    #This is a list of functions object returned by this makeCacheMatrix function
    #here it returns the matrix with our functions
    list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)

}




## This function computes the inverse of special matrix created by above functions.
# If the inverse of that particular matrix (same matrix as above, not changed) is already calculated  then our cacheSolve function will return the cached inverse matrix and skip the computations below, otherwise it computes the inverse of a matrix and set the cach values too.

cacheSolve <- function(x, ...) {
        ## Return the inverse of a matrix
        matrixInverse <- x$getInverse()
        
        
        ##Checks if th Inverse is calculated or not.  If yes display the message and return the cached value of object matrixInverse  and the function cacheSolve exit. If no the inside statement of if loop will not run and the staements after it will run.
        if(!is.null(matrixInverse)) {
            message("getting cached data")
            return(matrixInverse)
        }
        
        #As inverse is not calculated, we will calculate it here
        data <- x$get()
        matrixInverse <- solve(data, ...)#compute the inverse of data. solve is a function in R to calculate inverse of a matrix
        x$setInverse(matrixInverse)
        matrixInverse
}
