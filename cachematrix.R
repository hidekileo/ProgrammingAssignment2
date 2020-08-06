## first function : MakeCacheMatrix ()
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
