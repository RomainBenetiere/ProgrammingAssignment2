## Those functions creates special "matrix" objects and compute inverse of the 
## matrix.
## Matrix and inverse of the matrix are then cached in mermory.
## When cacheSolve is launched, it first checks if the matrix to compute is the
## same as the one in memory . If it's the same it just retrieves the inverse 
## from the cache. If it's not the same, solve calculation is applyed, values 
## are cached in memory and inverse value is retrieved.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL
        ## NULL is assigned to minv
        
        set <- function(y) {
                x <<- y
                ## When $set subfunction is called, x from the parent function takes 
                ## the value of y defined in this sub-function. Matrix value is cahched
                minv <<- NULL
                ## When $set subfunction is called, NULL is assigned to minv. NULL is
                ## cached insted of inverse of the matrix
        }
        
        get <- function() {
                ## When $get subfunction is called, value of x is returned from cache
                x
        }
        
        setminv <- function(matinv) {
                minv <<- matinv
                ## When $setminv subfunction is called, minv from the parent function 
                ## takes the value of matinv defined in this sub-function. Inverse of 
                ## the matrix is cached
        }
        
        getminv <- function() {
                minv
                ## When $getminv subfunction is called, minv value is returned from
                ## cache
        }
        
        list(set=set, get=get, setminv=setminv, getminv=getminv)
        ## List of subfunction  is returned for parent function
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        minv <- x$getminv()
        ## $getminv subfunction is called for the precised argument. Cached
        ## value for the inverse of the matrix is attributed to minv object
        
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        ## If cached valuet fot the inverse of the matrix is not NULL, then
        ## the message "getting cached data" is printed and the value is
        ## is returned
        
        data <- x$get()
        ## $get subfunction is called for the precised argument. Cached
        ## value for matrix is attributed to data object
        
        minv <- solve(data, ...)
        ## solve function is called and applied on data object. Value of the 
        ## computation is attributed to minv object (minv:inverse of the matrix)
        
        x$setminv(minv)
        ## $setminv subfunction is called to set the value of minv in the cache
        
        minv
        ## value of minv object is printed
}