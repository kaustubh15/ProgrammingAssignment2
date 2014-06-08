
## The method makeCacheMatrix is used create a list of functions to set the value of the matrix,
## to get the value of the matrix, to set the value of the inverse of the matrix and to get 
## the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        # Set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # Get the value of the matrix
        get <- function() {
                x
        }
        # Set the value of the inverse of the matrix
        setInverse <- function(inverse) {
                m <<- inverse
        }
        # Get the value of the inverse of the matrix 
        getInverse <- function() {
                m
        }
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The function cacheSolve is used to check if the inverse of the matrix is already computed
## or not. If it is, the function returns it from the cache. If it is not, it computes it and
## and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(x)) {
                message("Retrieving the cached results")
                return m
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}
