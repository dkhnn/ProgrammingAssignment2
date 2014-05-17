## The below functions provide a method for using a matrix with a cached inverse
## The inverese of a function as a costly computation may be cached with the below 
## two functions to provide a faster means of it's computation

## The below funtion recieves a matrix as a prameter and returns a matrix with a 
## cached inverse possibility

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The below function when applied to a matrix created with the makeCacheMatrix 
## function will return it's inverse. In case this function is invoked for the first time
## the inverse is calculated and cached. Consecutive invocations of this functions 
## on the same matrix will return the cached value thus limiting redundant computations

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
