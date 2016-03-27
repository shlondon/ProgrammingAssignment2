##The following functions cache the inverse of a invertible matrix

## makeCahceMatrix creates a special "Matrix" object that can
## cache its inverse. In other words it is a function that stores
## four function, 1st. allow you set a matrix object, 2nd. allow you
## get a matrix object, 3rd. allow you set inverse of a matrix,
## 4th. allow you get inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function calculates the inverse of a matrix created 
## with makeCacheMatrix function. It first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse
## of the data and set the inverse in the cache the setinverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
