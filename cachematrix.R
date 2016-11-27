## Below are two functions that are used to create a special object that stores a numeric matrix
# and caches/returns its inverse

## The makeCacheMatrix function creates a list of 4 functions that
## contain and set the matrix object and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The cacheSolve takes the special list created in the above function and returns the inverse
## the matrix by using the cached inverse if available in the list or caclulating it if it's not

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)   ## Return a matrix that is the inverse of 'x' if cached
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv  ## Return a calculated matrix that is the inverse of 'x'
       
}
