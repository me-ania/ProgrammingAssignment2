## The two functions below are used to create an object that stores a matrix and 
##cache's it's inverse

## The first function called makeCacheMatrix creates a list of functions
##to set the value of the matrix, get the value of the matrix, set the value of 
## the inverse and get the value of the inverse.
##

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The function cacheSolve calculates the inverse of the matrix.  It first 
##attempts to find the inverse in the cache and if found it displays the cached 
#data.  If not found, it calculates the inverse, sets the value in the
##cache using the setmean function and displays the inverted matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
       
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

