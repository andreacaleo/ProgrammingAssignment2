## Creates functions that calculate the inverse of a square matrix. The input matrix must be
## square and invertible. The inverse matrix is saved in cache and computed only once.


## makeCacheMatrix creates a special version of a matrix which is actually composed of  
## a set of functions to set/get the matrix, and to set/get the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y){
        x<<- y
        inverse <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inv) inverse <<- inv
    
    getInverse <- function() inverse
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve returns the inverse of a matrix. If a matrix is passed as an argument more than once,
## the value is retrieved from cache rather than calculated twice.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    
    if (!is.null((inverse))){
        message("Getting cached inverse matrix.")
        return(inverse)
    }
    
    original_matrix <- x$get()
    inverse <- solve(original_matrix)
    x$setInverse(inverse)
    inverse
}
