## The following set of functions work for a square matrix
## The functions create and cache the inverse of a square input matrix
## When cache of input matrix exists it is returned from cache otherwise calculated

## This function creates a special "matrix" object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the inverse
        inv <- NULL
        
        ## Cache the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        
        ## cache the inverse of matrix
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        
        ## Check if inverse exists is in cache
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)

        ## Return a matrix that is the inverse of 'x' i.e., the input
        inv
}
