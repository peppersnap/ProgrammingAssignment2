## Functions to create and update a matrix that caches its inverse

## creates a list of functions to get and set a matrix
## and also get/set a cached inverse of this matrix

makeCacheMatrix <- function(matrix.value = matrix()) {
    cached.inverse <- NULL 
    set <- function(new.matrix) {
        matrix.value <<- new.matrix
        cached.inverse <<- NULL ## clear cached inverse value when matrix is changed
    }
    get <- function() matrix.value
    setinverse <- function(new.inverse) cached.inverse <<- new.inverse
    getinverse <- function() cached.inverse 
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## takes the inverse of matrix 'x' from its cache if the cache is not null,
## otherwise calculates the inverse and sets x's cache

cacheSolve <- function(cache.matrix, ...) {
        ## Return a matrix that is the inverse of 'x'
    matrix.inverse <- cache.matrix$getinverse()
    if(!is.null(matrix.inverse)) { 
        message("getting cached data")
        return(matrix.inverse)
    }
    
    ## if the cache was null, solve the matrix then update its cache
    matrix.value <- cache.matrix$get()
    matrix.inverse <- solve(matrix.value)
    cache.matrix$setinverse(matrix.inverse)
    matrix.inverse
}
