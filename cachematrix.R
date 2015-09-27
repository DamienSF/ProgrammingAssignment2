## Computing the inverse of a matrix can be time consuming.
## Caching the inverse of the matrix can be a way to make the computation more effective.
## The two functions below can compute and cache the inverse of a matrix. 

## makeCacheMatrix creates a special "matrix" which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve returns the inverse of the special "matrix" created with makeCacheMatrix
## It first checks if the inverse of the matrix has already been computed.
## If so, it gets the result and skips the computation.
## Otherwise, it computes the inverse of the special "matrix" and sets the value in the cache via setinv function.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}