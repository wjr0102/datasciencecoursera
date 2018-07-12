## These functions is used to do cache matrix inverse
## makeCacheMatrix function is used to create cache matrix
## cacheSolve function is used to get or calculate inverse

## makeCacheMatrix creates a special 'matrix' containing functions
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the matrix inverse
##  4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list( set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## cacheSolve return the matrix inverse
##  1. If it was calculated before, return it
##  2. Otherwise, calculate and store the inverse 

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
