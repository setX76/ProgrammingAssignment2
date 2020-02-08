

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- 0
        }
        get <- function() x
        setinvers <- function(solve) m <<- solve
        getinvers <- function() m
        list(set = set, get = get,
             setinvers = setinvers,
             getinvers = getinvers)
}


## This function computes the inverse of the special "matrix" 
## returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinvers()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvers(m)
        m
}

