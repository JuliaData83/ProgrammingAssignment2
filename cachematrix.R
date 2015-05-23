## makeCacheMatrix creates a matrix object that can cache its
## inverse. cacheSolve checks to see if the inverse of the
## matrix has already been calculated and if it has, it pulls
## it from the cache and returns it

## the below function creates a matrix object and a vector that
## is a list of the four functions needed to calculate the inverse
## and cache the calculation


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## this function calculates the inverse of the matrix created by
## the above function. it first checks to see if the inverse
## has already been calculated and if so, it retrieves it from the 
## cache. Otherwise, it performs the calculation and returns the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
