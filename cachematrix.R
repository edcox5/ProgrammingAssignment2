## Write a pair of functions that calculate and cache the inverse of a matrix

## function makeCacheMatrix 
## creates a special "matrix", which is really a list containing the following 
## methods:
##   set the value of the matrix
##   get the value of the matrix
##   set the value of the inverse of the matrix
##   get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) { # sets the value of the matrix
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## function cacheSolve
## Returns a matrix that is the inverse of 'x'
##
## Calculates the mean of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the matrix and sets the value of the inverse in 
## the cache via the setinverse method.
##
## THIS FUNCTION ASSUMES THAT THE SUPPLIED MATRIX IS INVERTIBLE

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached inverse matrix")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}