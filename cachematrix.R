## first function : MakeCacheMatrix ()
##This first function creates a specific object of a matrix
MakeCacheMatrix <- function(x = matrix()){
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##Second function : CacheSolve ()
##This function get the inverse of the cache data
CacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("Getting Cached Data...")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
