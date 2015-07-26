## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 invx <- NULL
        set = function(y) {
                x <<- y
                invx <<- NULL
        }
        
        get <- function() x
        invset <- function(inv) invx <<- inv
        invget <- function () invx
        list(set = set, get = get,
             invset = invset,
             invget = invget)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$invset()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solution(data, ...)
        x$invset(m)
        m
}
