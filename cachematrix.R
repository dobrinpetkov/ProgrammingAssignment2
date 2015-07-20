## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## 'x' is a matrix that will be cached together with its inverse
    
    ## Return a list with functions get, set, get_inverse, set_inverse
    
    x_inverse <- NULL
    
    set <- function(y) {
        x <<- y
        x_inverse <<- NULL
    }
    
    get <- function() x
    
    set_inverse <- function(y) {
        x_inverse <<- y 
    }
    
    get_inverse <- function() x_inverse
    
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## This function gets an inverse of matrix stored in x 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x_inverse <- x$get_inverse()
    if(!is.null(x_inverse)) {
        message("getting cached data")
        return(x_inverse)
    }
    data <- x$get()
    x_inverse <- solve(data, ...)
    x$set_inverse(x_inverse)
    x_inverse
}
