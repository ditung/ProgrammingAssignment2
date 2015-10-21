##
## Put comments here that give an overall description of what your
## functions do

## Creates a list with an embedded matrix, that can be cached

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        
        ## If called, changes embedded matrix
         set <- function(y) {
                x <<- y
                m <<- NULL
         }
        
        ## Retrieves matrix that has been cached
        get <- function() x
        
        ## Caches value of inverse
        setinv <- function(inv) m <<- inv
        
        ## Recalls cached value of inverse
        getinv <- function() m
        
        ## Creates list of functions with matrix embedded
        list(get = get,
             setinv = setinv,
             getinv = getinv)
        
}

## Return a matrix that is the inverse of 'x,' which is actually a list

cacheSolve <- function(x, ...) {
        
        m <- x$getinv()
        
        ## Checks if inverse of matrix has been cached
        ## If so, returns inverse and exits functions
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        
        ## If not, solve for inverse, cache it and return it
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m

}




