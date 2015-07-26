## July 26, 2015
## Class r-prog030
##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
 ## initializing the invx
        invx <- NULL
        set = function(y) {
                x <<- y
                invx <<- NULL
        }
         
        get <- function() x                  ## Obtaining the matrix
        invset <- function(inv) invx <<- inv ## setting inverse matrix
        invget <- function () invx           ## getting inverse matrix
        ## Listing the list
        list(set = set, 
             get = get,
             invset = invset,
             invget = invget)
}
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        ## computing the inverse matrix
        m <- x$invget()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        ##data <- x$get()
        m <- solution(x$get(), ...)
        x$invset(m) ## setting the inverse to the object
        m
}
