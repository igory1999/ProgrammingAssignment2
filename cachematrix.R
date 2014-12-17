## The program provides tools to cache the computation of an inverse matrix.

## Usage:
##   1) Create a matrix object with makeCacheMatrix function. 
##      For example, m = makeCacheMatrix(c(1,2,9,3), 2, 2).
##   2) Find an inverse: inv = cacheSolve(m)
## During the first call to cacheSolve the inverse is actually computed. 
## In all the subsequent calls on the same matrix object the cached value is used.

## makeCacheMatrix function constructs a container to store matrix and its inverse. 
## It also provides the corresponding set/get methods.
## To construct an object, pass a matrix as an argument to makeCacheMatrix . 
## In subsequent use, the object is accessed via get/set/getinv/setinv methods.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve takes as an argument an object returned by makeCacheMatrix and 
## checks if the inverse of the matrix has already been computed.
## If not, it is computed now and is stored in makeCacheMatrix object, 
## otherwise, the cached value is returned.
## If a cached value is used, a message is printed.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
