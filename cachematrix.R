## Cache the inverse of a matrix and retreive it

## Cache the inverted matrix

makeMatrix <- function(x = matrix()) {
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
cacheMatrix <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached matrix")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
}

