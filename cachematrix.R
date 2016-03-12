## These function are used to save time in repetitive calculations with the same matrix
## In this case, using scoping rules, an inverse matrix calculation is stored inside
## an object and it will be used in a different environment.

## makeCacheMatrix creates a special matrix with some properties and functions to get or set a
## to be cached and used in a differen environment. In this case the value is
## the inverse matrix calculated with solve.

makeCacheMatrix <- function(x = matrix()) {
    s<-NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
  
    get<-function() x
    setsolve<- function(solve) s <<- solve
    getsolve<- function() s
    list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve get the inverse of a Matrix (the special matrix from makeCacheMatris)
## but at first it checks if it was calculated previously from the same matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
            message("getting cached data")
            return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
