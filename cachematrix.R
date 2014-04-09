## This function creates a special "matrix", which is really a list containing
## a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the inverse of the matrix
## - get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then cachesolve retrieves the inverse from
## the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (is.null(inv)) {
                m <- x$get()
                inv <- solve(m)
                x$setinverse(inv) 
        }
        inv
}
