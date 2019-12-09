## a pair of functions that calculate and cache 
## the inverse of a matrix.


## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## 1.  set the value of the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ## 2.  get the value of the matrix
    get <- function() x
    ## 3.  set the value of the inverse
    setinverse <- function(inverse) i <<- inverse
    ## 4.  get the value of the inverse
    getinverse <- function() i
    ## aggregate outputs to a list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse 
## has already been calculated (and the matrix has not changed),
## then `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    ## Grab cache inverse if not new matrix
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## if new matrix, calculate inverse
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
