## So these bad boys are used to cache the inverse of a matrix (a potentially 
## time-consuming computation) with the original matrix object. For a large
## matrix, it may take a while to compute the inverse, especially if it has to 
## be computed repeatedly (e.g. in a loop). If the contents of a matrix are not 
## changing, it may make sense to cache the value of the inverse so that when 
## we need it again, it can be looked up in the cache rather than recomputed.

## The first function, makeCacheMatrix creates a "special matrix" (hereunto 
## known as "uber-matrix") and a list containing functions to:
##  
## 1) set the value of the uber-matrix - combines the original matrix and a 
##    blank cache object (function: set)
## 2) return the uber-matrix object (function: get)
## 3) store the calculated inverse in the uber-matrix cache 
##    (function: setinverse)
## 4) return the value of the inverse stored in the uber-matrix cache
##    (function: getinverse)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function calculates the inverse of the uber-matrix created 
## with the above function. However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the data and sets 
## the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
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
