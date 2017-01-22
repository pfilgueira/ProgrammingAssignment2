## Functions with the purpose of caching the inverse Matrix and retrieving 
## it from the cache in case of a new Matrix

## makeCacheMatrix creates a special Matrix with the following purpose:
##      a. Set the value of the Matrix
##      b. Get the value of the Matrix
##      c. Set the value of the inverse Matrix
##      d. Get the value of the inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverseMatrix <<- solve
        getinverse <- function() inverseMatrix
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)        
}


## cacheSolve retrieves the inverse matrix from the cache in case it's available
## if not available, calculates the inverse of matrix using the solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getinverse()
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
        data <- x$get()
        inverseMatrix <- solve(data, ...)
        x$setinverse(inverseMatrix)
        inverseMatrix        
}