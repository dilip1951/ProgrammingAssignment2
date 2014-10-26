## Put comments here that give an overall description of what your
## functions do

# This function puts the matrix in cache, calculates the inverse.
# It puts the matri and the associated inverse in the cache.

makeCacheMatrix <- function(X= matrix()) {
        inv <- NULL
        set <- function(Y) {
                X <<- Y
                inv <<- NULL
        }
        get <- function() X
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

# The following function checks if the matrix and its inverse exist in the cache.
# If they do, it returns the value of inverse from the cache.
# Otherwise it calculates the inverse of the matrix.
solve <- function(X, ...) {
        inv <- X$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- X$get()
        inv <- solve(data, ...)
        X$setinv(inv)
        inv
}

