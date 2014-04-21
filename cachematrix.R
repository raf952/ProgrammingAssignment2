## Similar to the example makeVector and cachemean functions, these functions
## provide similar functionality for matrix inversion using the R solve function

# This function creates a special "matrix" object that can cache its inverse.
## The first function, makeVector creates a special "vector", which is really
## a list containing a function to
## 
## set the value of the vector
## get the value of the vector
## set the value of the inverse
## get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() {x}
        
        setinverse <- function (solve) i <<- solve
        getinverse <- function() i
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##  This function computes the inverse of the special "matrix" returned by
## `makeCacheMatrix` above. If the inverse has already been calculated (and the
## matrix has not changed), then the `cachesolve` should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}