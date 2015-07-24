## The first function 'makeCacheMatrix' returns a matrix with extended functionality for caching its inverse

## The second function 'cacheSolve' returns the inverse of matrix stored in 'makeCacheMatrix' object. If the 
## inverse is already calculated and cached, 'cacheSolve' just fetches it, otherwise it calculates the inverse
## matrix and caches the inverse in the 'makeCacheMatrix' object if needed later.



## 'makeCacheMatrix' creates an object where a matrix and its inverse could be stored.

makeCacheMatrix <- function(x = matrix()) {
    ## 'x' is a matrix that will be cached together with its inverse
    ## The interface is defined by the functions get, set, get_inverse, set_inverse

    x_inverse <- NULL
    
    
    ## interface functions
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


## This function gets an inverse of matrix stored in x if it is available or calculates the inverse and caches 
## it in the 'makeCahceMatrix' object if it wasn't calculated before
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    x_inverse <- x$get_inverse()
    
    ## use cached inverse if it is avaliable
    if(!is.null(x_inverse)) {
        message("getting cached data")
        return(x_inverse)
    }
    
    # calculate the inverse and cache it if it wasn't available
    data <- x$get()
    x_inverse <- solve(data, ...) ## calculate inverse
    x$set_inverse(x_inverse)      ## cache the inverse
    x_inverse
}
